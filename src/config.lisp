(in-package :cl-scrobbler)


;;;; Library Configuration

;; All queryparams should be utf-8 encoded.
(setf drakma:*drakma-default-external-format* :utf-8)
;; tell Drakma to handle JSON as strings
(pushnew '("application" . "json") drakma:*text-content-types*
         :test (lambda (x y)
                 (and (equalp (car x) (car y))
                      (equalp (cdr x) (cdr y)))))

;;;; Last.fm API Configuration
(defvar *api-version* "2.0"
  "The current Last.fm API version.")
(defvar *api-url* (format nil "http://ws.audioscrobbler.com/~D/" *api-version*)
  "The URL for Last.fm API calls.")
(defparameter *api-key* "33019c2bf2e0ef2c78893ac864a94f20"
  "A valid Last.fm API key.")
(defparameter *api-secret* "c1374522165ddf37486a4ed20f2af335"
  "A valid Last.fm API secret key.")

(defparameter *config-dir* ""
  "The directory to store the session key, scrobble cache and log in.")
(defparameter *session-key* nil
  "The session key used to authenticate calls to last.fm.")

;;;; Cl-Scrobbler Configuration
(defvar *scrobble-count* 5
  "The number of songs needed in the queue before an attempt to scrobble is
made.")

(defvar *scrobble-p* nil
  "This variable determines whether or not songs are added to the scrobble
cache.")

(defvar *now-playing-p* nil
  "This variable determines whether the user's now playing status is updated.")

(defun save-settings ()
  "Persist the *SCROBBLE-COUNT*, *SCROBBLE-P* and *NOW-PLAYING-P* settings."
  (with-open-file (out (config-file "settings")
                       :element-type '(unsigned-byte 8)
                       :direction :output :if-exists :supersede
                       :if-does-not-exist :create)
    (cl-store:store (list *scrobble-count* *scrobble-p* *now-playing-p*) out)))

(defun restore-settings ()
  "Restore the *SCROBBLE-COUNT*, *SCROBBLE-P* and *NOW-PLAYING-P* variables."
  (with-open-file (in (config-file "settings")
                      :element-type '(unsigned-byte 8))
    (destructuring-bind (count scrobble now-playing) (cl-store:restore in)
      (setf *scrobble-count* count
            *scrobble-p* scrobble
            *now-playing-p* now-playing))))
