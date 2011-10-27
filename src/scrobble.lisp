(in-package :cl-scrobbler)


;;;; Finally! The good stuff...

(defvar *last-seek* 0
  "The destination position in seconds of the last seek in the current track.")

(defvar *skipped* nil
  "Either NIL or the position in seconds before the user skipped to the next
track.")

(defvar *song-info* nil
  "A list of (track artist length timestamp) of type (string string int string)
or NIL.")

(defvar *song-info-fn* nil
  "This is a lambda or named function intended to take no arguments and return
a list where the first element is the track name, the second is the artist and
the third is the track length in seconds.")

(defvar *song-time-fn* nil
  "This is a lambda or named function intended to take no arguments and return
the current position in the track in seconds.")

(defcall "track.updateNowPlaying" (track artist sk)
  (:method :post :docs "Update the Now Playing status on last.fm.")
  (getjso "nowplaying" json))

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
scrobbling when a connection is reestablished."
  (when (and *scrobble-p* *song-info* (valid-scrobble-p))
    (destructuring-bind (track artist length timestamp) *song-info*
      (add-to-cache (list track timestamp artist))))
  (setf *song-info* nil
        *skipped* nil
        *last-seek* 0))

(defun toggle-scrobbling ()
  "Toggle whether or not new songs are added to the queue."
  (unless *session-key*
    (get-session-key))
  (prog1 (setf *scrobble-p* (not *scrobble-p*))
    (save-settings)))

(defun toggle-now-playing ()
  "Toggle whether or not the Now Playing status is updated with each song."
  (unless *session-key*
    (get-session-key))
  (prog1 (setf *now-playing-p* (not *now-playing-p*))
    (save-settings)))

(defun set-now-playing ()
  "Update the now playing status when *NOW-PLAYING-P* is non-NIL."
  (when *now-playing-p*
    (update-now-playing (first *song-info*) (second *song-info*) *session-key*)))

(defun update-last-seek ()
  "Set *LAST-SEEK* to the current track position via *SONG-TIME-FN*."
  (setf *last-seek* (funcall *song-time-fn*)))

(defun update-song-info ()
  "Set *SONG-INFO* to a list of (track artist duration timestamp) via
*SONG-INFO-FN* and UNIX-TIMESTAMP."
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
        (error 'scrobble-error))))

(defun scrobbler-init ()
  "Ensure needed variables are set, restore the cache and settings if present
and, if scrobbling is enabled, restore or acquire a session key."
  (unless (and *song-info-fn* *song-time-fn*)
    (error "You need to define *song-time-fn* and *song-info-fn*! ~
Consult the docs..."))
  (when (probe-file (config-file "settings"))
    (restore-settings))
  (when (probe-file (config-file "cache"))
    (restore-cache))
  (when (or *scrobble-p* *now-playing-p*)
    (get-session-key)))

(defun scrobbler-loop ()
  "Loop indefinitely. If there are at least *SCROBBLE-COUNT* songs queued,
scrobble until the queue is empty or errors occur. Sleep 2 minutes and repeat."
  (loop
     (when (>= (queue-count *scrobble-cache*) *scrobble-count*)
       (handler-case (loop until (queue-empty-p *scrobble-cache*)
                        do (attempt-scrobble))
         (scrobble-error () nil)))
     (sleep 120)))
