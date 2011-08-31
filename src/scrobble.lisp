(in-package :cl-scrobbler)


;;;; Finally! The good stuff...

(defun valid-scrobble-p (track-length seconds-played)
  "Last.fm defines a valid scrobble as a track that is over 30 seconds long
which has been played for over half its length OR 4 minutes."
  (and (>= track-length 30)
       (or (> seconds-played (/ track-length 2))
           (> seconds-played 240))))

(defcall "track.updateNowPlaying" (track artist sk)
  (:method :post :docs "Update the Now Playing status on last.fm."))

(defcall "track.scrobble" (track timestamp artist sk)
  (:method :post :docs "Scrobble the track!")
  (getjso "scrobbles" json))

(defun queue-scrobble (track artist timestamp &key now-playing-p)
  "Add a song to the queue to be scrobbled. If there is a network failure, the
track will be stored for later scrobbling when a connection is reestablished."
  (when now-playing-p
    (update-now-playing track artist *session-key*))
  (add-log-entry :path "scrobbler-plays.log" "[~d]> Played ~a by ~a"
                 timestamp track artist)
  (add-to-cache 'scrobble (list track timestamp artist *session-key*)))
