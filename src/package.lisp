(defpackage #:cl-scrobbler
  (:use #:cl)
  (:import-from #:st-json #:read-json
                          #:getjso)
  (:export #:*config-dir*
           #:*scrobble-cache*
           #:restore-cache
           #:get-session-key
           #:valid-scrobble-p
           #:update-now-playing
           #:scrobble))

