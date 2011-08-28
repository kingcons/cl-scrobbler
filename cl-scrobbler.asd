;;;; cl-scrobbler.asd

(asdf:defsystem #:cl-scrobbler
  :name "cl-scrobbler"
  :description "A library for scrobbling to last.fm"
  :version "0.1"
  :author "Brit Butler <redline6561@gmail.com>"
  :depends-on (#:md5 #:drakma #:cl-store #:st-json)
  :components ((:module src
                :serial t
                :components ((:file "package")
                             (:file "config")
                             (:file "cache")
                             (:file "errors")
                             (:file "util")
                             (:file "auth")
                             (:file "scrobble")))))
