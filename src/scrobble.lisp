(in-package :cl-scrobbler)


;;;; Finally! The good stuff...

(defvar *last-seek* 0
  "The destination position in seconds of the last seek in the current track.")

(defvar *song-info* nil
  "A list of (track artist length timestamp) of type (string string int string)
or Nil.")

(defvar *song-info-fn* nil
  "This is a lambda or named function intended to take no arguments and return
a list where the first element is the track name, the second is the artist, the
third is the track length in seconds and the fourth is the timestamp from when
it began playing.")

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
         (seconds-played (- song-length *last-seek*)))
    (with-open-file (out (config-file "debug") :direction :output
                         :if-exists :append :if-does-not-exist :create)
      (format out "Song timing was ~a, song was ~a~%~%"
              (list song-length seconds-played) (funcall *song-info-fn*)))
    (and (>= song-length 30)
         (or (> seconds-played (/ song-length 2))
             (> seconds-played 240)))))

(defun maybe-queue-scrobble ()
  "When valid (as determined by VALID-SCROBBLE-P), add a song to the queue to be
scrobbled. If there is a network failure, the track will be stored for later
scrobbling when a connection is reestablished. If *NOW-PLAYING-P* is set, also
set the user's now playing status."
  (let ((valid-p (and *song-info* (valid-scrobble-p)))
        (track (first *song-info*))
        (artist (second *song-info*)))
    (when valid-p
      (when *now-playing-p*
        (update-now-playing track artist *session-key*))
      (add-to-cache (list track (fourth *song-info*) artist *session-key*)))
    (setf *song-info* nil)))

(defun update-last-seek ()
  "Set *LAST-SEEK* to the current track position via *SONG-TIME-FN*."
  (setf *last-seek* (funcall *song-time-fn*)))

(defun update-song-info ()
  "Set *SONG-INFO* to a list of (track artist duration) via *SONG-INFO-FN*."
  (setf *song-info* (append (funcall *song-info-fn*) (list (timestamp)))))

(defun attempt-scrobble ()
  "Peek at *SCROBBLE-CACHE* and attempt to scrobble the next song. If
successful, remove the song from the cache and persist it to disk."
  (let* ((song (peek-queue *scrobble-cache*))
         (result (apply #'scrobble song)))
    (when result
      (remove-from-cache))))

(defun scrobbler-init ()
  "Loop indefinitely, scrobbling as many songs as possible unless the network
is unavailable. Then sleep until at least *SCROBBLE-COUNT* songs are queued."
  (unless (and *song-info-fn* *song-time-fn*)
    (error "You need to define *song-time-fn* and *song-info-fn*! ~
Consult the docs..."))
  (when (probe-file (config-file "cache"))
    (restore-cache))
  (get-session-key)
  (loop
     ;; TODO: Probably want some HANDLER-CASE magic here...
     (when (and *scrobble-p* (>= (queue-count *scrobble-cache*)
                                 *scrobble-count))
       (loop until (queue-empty-p *scrobble-cache*)
          do (attempt-scrobble)))
     (sleep 300)))
