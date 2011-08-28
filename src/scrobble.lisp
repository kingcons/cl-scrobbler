(in-package :cl-scrobbler)

(defun valid-scrobble-p (track-length seconds-played)
  "Last.fm defines a valid scrobble as a track that is over 30 seconds long
which has been played for over half its length OR 4 minutes."
  (and (>= track-length 30)
       (or (> seconds-played (/ track-length 2))
           (> seconds-played 240))))

(defcall "track.updateNowPlaying" (track artist sk)
  (:method :post :docs "Update the Now Playing status on last.fm. If the service
cannot be reached no caching or subsequent attempts are made."))

(defcall "track.scrobble" (track timestamp artist sk)
  (:method :post :docs "Scrobble the track or put it in the cache if last.fm
could not be reached."))
