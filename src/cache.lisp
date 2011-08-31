(in-package :cl-scrobbler)

(defvar *scrobble-cache* (make-queue)
  "A queue of cached scrobbles to send when last.fm is available.")

(defun cache-contents ()
  "Get a copy of the contents of the *SCROBBLE-CACHE*."
  (loop for song in (queue-elements *scrobble-cache*) collecting song))

(defun add-to-cache (scrobble)
  "Add the attempted SCROBBLE to the cache and serialize it to disk."
  (enqueue *scrobble-cache* scrobble)
  (with-open-file (out (config-file "cache") :direction :output
                       :if-does-not-exist :create :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    (cl-store:store *scrobble-cache* out)))

(defun restore-cache ()
  "Restore the cache from disk."
  (with-open-file (in (config-file "cache") :element-type '(unsigned-byte 8))
    (setf *scrobble-cache* (cl-store:restore in))))
