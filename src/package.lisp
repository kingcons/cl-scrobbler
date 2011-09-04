(defpackage #:cl-scrobbler
  (:use #:cl)
  (:import-from #:st-json #:read-json
                          #:getjso)
  (:export #:*config-dir*
           #:*song-info-fn*
           #:*song-time-fn*
           #:*scrobble-p*
           #:*scrobble-count*
           #:*now-playing-p*
           #:update-last-seek
           #:update-song-info
           #:update-skipped
           #:maybe-queue-scrobble
           #:scrobbler-init
           #:cache-contents))
