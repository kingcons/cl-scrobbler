(in-package :cl-scrobbler)

(defvar *scrobble-cache* (make-queue)
  "A queue of cached scrobbles to send when last.fm is available.")

(defun cache-contents ()
  "Get a copy of the contents of the *SCROBBLE-CACHE*."
  (loop for song in (queue-elements *scrobble-cache*) collecting song))

(defun add-to-cache (scrobble)
  "Add the attempted SONG to the cache and serialize it to disk."
  (enqueue *scrobble-cache* scrobble)
  (with-open-file (out (config-file "cache") :direction :output
                       :if-does-not-exist :create :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    (cl-store:store *scrobble-cache* out)))

(defun restore-cache ()
  "Restore the cache from disk."
  (with-open-file (in (config-file "cache") :element-type '(unsigned-byte 8))
    (setf *scrobble-cache* (cl-store:restore in))))

(defmacro with-caching ((call params) &body body)
  "Execute BODY in a restart-case such that if CALL is in *CACHED-METHODS* and
a condition is encountered, a cache-it restart is available to invoke
ADD-TO-CACHE with CALL and PARAMS."
  `(restart-case (progn ,@body)
     (cache-it ()
       (add-to-cache ,call ,params))))
