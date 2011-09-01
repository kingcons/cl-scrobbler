(in-package :cl-scrobbler)


;;;; Finally! The good stuff...

(defvar *last-seek* 0
  "The destination position in seconds of the last seek in the current track.")

(defvar *scrobble-count* 5
  "The number of songs needed in the queue before an attempt to scrobble is
made.")

(defvar *scrobble-p* nil
  "This variable is checked to determine whether the plugin is enabled.")

(defvar *now-playing-p* nil
  "This variable determines whether the user's now playing status is updated.")

(defvar *track-info-fn* nil
  "This is a lambda or named function intended to take no arguments and return
a list where the first element is the track name and the second is the artist.")

(defvar *track-time-fn* nil
  "This is a lambda or named function intended to take no arguments and return
a list where the first element is the track length and the second is the
current position in the track.")

(defcall "track.updateNowPlaying" (track artist sk)
  (:method :post :docs "Update the Now Playing status on last.fm."))

(defcall "track.scrobble" (track timestamp artist sk)
  (:method :post :docs "Scrobble the track!")
  (getjso "scrobbles" json))

(defun valid-scrobble-p ()
  "Last.fm defines a valid scrobble as a track that is over 30 seconds long
which has been played for over half its length OR 4 minutes. We extend this
notion such that playtime must have occurred without seeking."
  (let* ((timing (funcall *track-time-fn*))
         (track-length (first timing))
         (seconds-played (- (second timing) *last-seek*)))
    (and (>= track-length 30)
         (or (> seconds-played (/ track-length 2))
             (> seconds-played 240)))))

(defun maybe-queue-scrobble ()
  "When valid (as determined by VALID-SCROBBLE-P), add a song to the queue to be
scrobbled. If there is a network failure, the track will be stored for later
scrobbling when a connection is reestablished. If *NOW-PLAYING-P* is set, also
set the user's now playing status."
  (unless (and *track-info-fn* *track-time-fn*)
    (error "You need to define *track-time-fn* and track-info-fn*! ~
Consult the docs..."))
  (let* ((timestamp (timestamp))
         (track-info (funcall *track-info-fn*))
         (track (first track-info))
         (artist (second track-info))
         (valid-p (valid-scrobble-p)))
  (when valid-p
    (when *now-playing-p*
      (update-now-playing track artist *session-key*))
    (add-to-cache (list track timestamp artist *session-key*)))))

(defun set-last-seek (&optional value)
  "If VALUE is supplied, set *LAST-SEEK* to that value, otherwise set it to 0."
  (setf *last-seek* (or value 0)))

(defun attempt-scrobble ()
  "Peek at *SCROBBLE-CACHE* and attempt to scrobble the next song. If
successful, remove the song from the cache and persist it to disk."
  (let ((song (queue-front *scrobble-cache*))
        (result (apply #'scrobble song)))
    (when result
      (remove-from-cache))))

(defun scrobbler-init ()
  "Loop indefinitely, scrobbling as many songs as possible unless the network
is unavailable. Then sleep until at least *SCROBBLE-COUNT* songs are queued."
  (when (probe-file (config-file "cache"))
    (restore-cache))
  (get-session-key)
  (loop
     ;; TODO: Probably want some HANDLER-CASE magic here...
     (loop while (> (length (queue-elements *scrobble-cache*))
                    *scrobble-count*)
        do (attempt-scrobble))
     (sleep 300)))
