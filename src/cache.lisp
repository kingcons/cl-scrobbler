(in-package :cl-scrobbler)

(defvar *scrobble-cache* (make-array 8 :adjustable t :fill-pointer 0)
  "A vector of cached scrobbles to send when network is available.")

