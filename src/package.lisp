(defpackage #:cl-scrobbler
  (:documentation "Homepage: <a href=\"http://github.com/redline6561/cl-scrobbler\">Github</a>")
  (:use #:cl)
  (:import-from #:st-json #:read-json
                          #:getjso)
  (:import-from #:arnesi #:queue
                         #:peek-queue
                         #:queue->list
                         #:queue-count
                         #:queue-empty-p
                         #:enqueue
                         #:dequeue)
  (:export #:*config-dir*
           #:*song-info-fn*
           #:*song-time-fn*
           #:*scrobble-count*
           #:update-last-seek
           #:update-song-info
           #:update-skipped
           #:set-now-playing
           #:toggle-scrobbling
           #:toggle-now-playing
           #:maybe-queue-scrobble
           #:scrobbler-init
           #:scrobbler-loop
           #:cache-contents))
