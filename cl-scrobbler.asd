(defsystem #:cl-scrobbler
  :name "cl-scrobbler"
  :description "A library for scrobbling to last.fm"
  :version "0.4"
  :license "BSD"
  :author "Brit Butler <redline6561@gmail.com>"
  :depends-on (#:md5 #:flexi-streams #:drakma #:st-json #:cl-store #:arnesi)
  :components ((:module src
                :serial t
                :components ((:file "package")
                             (:file "config")
                             (:file "cache")
                             (:file "errors")
                             (:file "util")
                             (:file "auth")
                             (:file "scrobble"))))
  :in-order-to ((test-op (load-op cl-scrobbler-tests)))
  :perform (test-op :after (op c)
                    (funcall (intern "RUN!" :cl-scrobbler-tests))))

(defsystem #:cl-scrobbler-tests
  :depends-on (cl-scrobbler fiveam)
  :pathname "tests/"
  :components ((:file "tests")))

(defmethod operation-done-p ((op test-op)
                             (c (eql (find-system :cl-scrobbler))))
  (values nil))
