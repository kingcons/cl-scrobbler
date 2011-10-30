(defpackage :cl-scrobbler-tests
  (:use :cl :cl-scrobbler :fiveam)
  (:export #:run!))

(in-package :cl-scrobbler-tests)

(def-suite :cl-scrobbler)
(in-suite :cl-scrobbler)

(test t-isnt-null
      (is (not (null t))))
