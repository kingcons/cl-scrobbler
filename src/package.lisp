(defpackage #:cl-scrobbler
  (:use #:cl)
  (:import-from #:st-json #:read-json
                          #:getjso)
  (:export #:*config-dir*
           #:*track-info-fn*
           #:*track-time-fn*
           #:*scrobble-p*
           #:*scrobble-count*
           #:*now-playing-p*
           #:set-last-seek
           #:maybe-queue-scrobble
           #:scrobbler-init
           #:cache-contents))
