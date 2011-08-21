;;;; package.lisp

(defpackage #:cl-scrobbler
  (:use #:cl)
  (:import-from #:st-json #:read-json
                          #:getjso))

