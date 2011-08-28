(in-package :cl-scrobbler)

(defvar *scrobble-cache* (make-array 8 :adjustable t :fill-pointer 0)
  "A vector of cached scrobbles to send when last.fm is available.")

(defvar *cached-methods* '("track.scrobble")
  "A list of Last.fm methods that should be cached for later resubmission in
case initial submission does not succeed.")

(defun add-to-cache (call params)
  "Add the attempted CALL and PARAMS to the cache and serialize it to disk."
  (vector-push-extend (cons call params) *scrobble-cache*)
  (with-open-file (out (config-file "cache") :direction :output
                       :if-does-not-exist :create :if-exists :supersede)
    (cl-store:store *scrobble-cache* out)))

(defun restore-cache ()
  "Restore the cache from disk."
  (with-open-file (in (config-file "cache"))
    (setf *scrobble-cache* (cl-store:restore in))))

(defmacro with-caching ((call params) &body body)
  "Execute BODY in a restart-case such that if CALL is in *CACHED-METHODS* and
a condition is encountered, a cache-it restart is available to invoke
ADD-TO-CACHE with CALL and PARAMS."
  `(restart-case (progn ,@body)
     (cache-it ()
       (when (member ,call *cached-methods* :test #'string=)
         (add-to-cache ,call ,params)))))
