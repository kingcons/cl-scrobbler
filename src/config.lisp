(in-package :cl-scrobbler)

;;;; Library Configuration

;; All queryparams should be utf-8 encoded.
(setf drakma:*drakma-default-external-format* :utf-8)
;
; tell Drakma to handle JSON as strings
(pushnew '("application" . "json") drakma:*text-content-types*
         :test (lambda (x y)
                 (and (equalp (car x) (car y))
                      (equalp (cdr x) (cdr y)))))

;;;; Last.fm API Configuration
(defvar *api-version* "2.0"
  "The current Last.fm API version.")
(defvar *api-url* (format nil "http://ws.audioscrobbler.com/~D/" *api-version*)
  "The URL for Last.fm API calls.")
(defvar *api-key* "33019c2bf2e0ef2c78893ac864a94f20"
  "A valid Last.fm API key.")
(defvar *api-secret* "c1374522165ddf37486a4ed20f2af335"
  "A valid Last.fm API secret key.")
(defvar *config-dir* ""
  "The directory to store the Session file and scrobble cache in.")

;;; User Specific Values
(defvar *username* ""
  "The Last.fm username to scrobble as.")
(defvar *password* ""
  "The password for *USERNAME*.")
(defvar *session-key* ""
  "The session key used to authenticate calls to last.fm.")
