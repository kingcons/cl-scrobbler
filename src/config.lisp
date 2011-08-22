(in-package :cl-scrobbler)

;; All queryparams should be utf-8 encoded.
(setf drakma:*drakma-default-external-format* :utf-8)
;
; tell Drakma to handle JSON as strings
(pushnew '("application" . "json") drakma:*text-content-types*
         :test (lambda (x y)
                 (and (equalp (car x) (car y))
                      (equalp (cdr x) (cdr y)))))

(defvar *submission-protocol-version* "1.2.1"
  "The version of the submission protocol to use. Only 1.2.1 is supported now.")
(defvar *submission-url* "http://post.audioscrobbler.com/"
  "The URL for Last.fm scrobble submissions.")
(defvar *api-version* "2.0"
  "The current Last.fm API version.")
(defvar *api-url* (format nil "http://ws.audioscrobbler.com/~D/" *api-version*)
  "The URL for Last.fm API calls.")

;; These values taken from http://www.last.fm/api/submissions#1.1
(defvar *client-id* "tst"
  "A Last.fm client identifier.")
(defvar *client-version* "1.0"
  "The published client version.")

(defvar *api-key* ""
  "A valid Last.fm API key.")
(defvar *api-secret* ""
  "A valid Last.fm API secret key.")
(defvar *config-dir* ""
  "The directory to store the Session file and scrobble cache in.")

(defvar *username* ""
  "The Last.fm username to scrobble as.")
(defvar *password* ""
  "The password for *USERNAME*.")

(defun config-file (name)
  "Return a pathname for NAME under *CONFIG-DIR*."
  (merge-pathnames name *config-dir*))

(defun get-api-keys (&key (path (config-file "auth")))
  "Set *API-KEY* and *API-SECRET* based on the contents of PATH.
It is expected that the first line is the key and the second line
is the secret."
  (with-open-file (in path)
    (setf *api-key* (read-line in nil)
          *api-secret* (read-line in nil))))
