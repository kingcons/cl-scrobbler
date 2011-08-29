(defpackage #:cl-scrobbler
  (:use #:cl)
  (:import-from #:st-json #:read-json
                          #:getjso)
  (:export #:*config-dir*
           #:cache-contents
           #:restore-cache
           #:get-session-key
           #:valid-scrobble-p
           #:queue-scrobble))
