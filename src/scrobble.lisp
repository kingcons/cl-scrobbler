(in-package :cl-scrobbler)


;;;; Finally! The good stuff...

(defvar *last-seek* 0
  "The destination position in seconds of the last seek in the current track.")

(defvar *skipped* nil
  "Either NIL or the position in seconds before the user skipped to the next
track.")

(defvar *song-info* nil
  "A list of (track artist length timestamp) of type (string string int string)
or Nil.")

(defvar *song-info-fn* nil
  "This is a lambda or named function intended to take no arguments and return
a list where the first element is the track name, the second is the artist and
the third is the track length in seconds.")

(defvar *song-time-fn* nil
  "This is a lambda or named function intended to take no arguments and return
the current position in the track in seconds.")

(defcall "track.updateNowPlaying" (track artist sk)
  (:method :post :docs "Update the Now Playing status on last.fm."))

(defcall "track.scrobble" (track timestamp artist sk)
  (:method :post :docs "Scrobble the track!")
  (getjso "scrobbles" json))

(defun valid-scrobble-p ()
  "Last.fm defines a valid scrobble as a track that is over 30 seconds long
which has been played for over half its length OR 4 minutes. We extend this
notion such that playtime must have occurred without seeking."
  (let* ((song-length (third *song-info*))
         (seconds-played (or *skipped* (- song-length *last-seek*))))
    (and (>= song-length 30)
         (or (>= seconds-played (/ song-length 2))
             (>= seconds-played 240)))))

(defun maybe-queue-scrobble ()
  "When valid (as determined by VALID-SCROBBLE-P), add a song to the queue to be
scrobbled. If there is a network failure, the track will be stored for later
scrobbling when a connection is reestablished. If *NOW-PLAYING-P* is set, also
set the user's now playing status."
  (let ((valid-p (and *song-info* (valid-scrobble-p)))
        (track (first *song-info*))
        (artist (second *song-info*)))
    (when (and *scrobble-p* valid-p)
      (add-to-cache (list track (fourth *song-info*) artist)))
    (setf *song-info* nil
          *skipped* nil
          *last-seek* 0)))

(defun toggle-scrobbling ()
  (setf *scrobble-p* (not *scrobble-p*))
  (format nil "Scrobbling is ~:[disabled~;enabled~]." *scrobble-p*))

(defun toggle-now-playing ()
  (setf *now-playing-p* (not *now-playing-p*))
  (format nil "Now playing status updates are ~:[disabled~;enabled~]." *now-playing-p*))

(defun update-now-playing ()
  "Update the now playing status when *NOW-PLAYING-P* is non-NIL."
  (when *now-playing-p*
    (update-now-playing (first *song-info*) (second *song-info*) *session-key*)))

(defun update-last-seek ()
  "Set *LAST-SEEK* to the current track position via *SONG-TIME-FN*."
  (setf *last-seek* (funcall *song-time-fn*)))

(defun update-song-info ()
  "Set *SONG-INFO* to a list of (track artist duration) via *SONG-INFO-FN*."
  (setf *song-info* (append (funcall *song-info-fn*)
                            (list (format nil "~d" (unix-timestamp))))))

(defun update-skipped ()
  "Set *SKIPPED* to the current track position via *SONG-TIME-FN*."
  (setf *skipped* (funcall *song-time-fn*)))

(defun attempt-scrobble ()
  "Peek at *SCROBBLE-CACHE* and attempt to scrobble the next song. If
successful, remove the song from the cache and persist it to disk."
  (let* ((song (peek-queue *scrobble-cache*))
         (result (apply #'scrobble (append song (list *session-key*)))))
    ;; Did we get JSON or did we get a logged error?
    (if (typep result 'st-json:jso)
        (remove-from-cache)
        (error 'network-outage))))

(defun scrobbler-init ()
  "Ensure needed variables are set, restore the cache if present and restore
or acquire a session key for scrobbling."
  (unless (and *song-info-fn* *song-time-fn*)
    (error "You need to define *song-time-fn* and *song-info-fn*! ~
Consult the docs..."))
  (when (probe-file (config-file "cache"))
    (restore-cache))
  (get-session-key))

(defun scrobbler-loop ()
  "Loop indefinitely, scrobbling as many songs as possible unless the network
is unavailable. Then sleep until at least *SCROBBLE-COUNT* songs are queued."
  (loop
     (when (>= (queue-count *scrobble-cache*) *scrobble-count*)
       (handler-case (loop until (queue-empty-p *scrobble-cache*)
                        do (attempt-scrobble))
         (network-outage () nil)))
     (sleep 120)))
