(in-package :cl-scrobbler)


;;;; User Authentication - http://www.last.fm/api/authentication
;;;; For now, we only support Desktop Auth. http://www.last.fm/api/desktopauth

;;; Request Tokens

(defcall "auth.getToken" ()
  (:docs "Get an Unauthorized Token for initiating an authenticated session.")
  (getjso "token" json))

(defun request-user-auth (token)
  "Ask the user to authorize cl-scrobbler to submit songs."
  (format t "Please visit the following link in your browser: ~
http://www.last.fm/api/auth/?api_key=~a&token=~a" *api-key* token))

;;; Authenticated Sessions

(defcall "auth.getSession" (token)
  (:docs "Get a Session Token for scrobbling. Last.fm recommends storing these
in a secure fashion as they generally have an infinite lifetime."))

;;; Putting it all together...

(defun authorize-scrobbling ()
  "Authorize cl-scrobbler to scrobble to an account, saving the session key in
a binary file and setting the *SESSION-KEY* global on success."
  (let ((token (get-token)))
    (request-user-auth token)
    (loop until (yes-or-no-p "Have you authorized cl-scrobbler?")
       do (request-user-auth token))
    (let ((session (getjso "session" (read-json (get-session token)))))
      (with-open-file (out (config-file "session") :direction :output
                           :if-exists :supersede :if-does-not-exist :create
                           :element-type '(unsigned-byte 8))
        (cl-store:store (mapcar (lambda (key)
                                  (getjso key session))
                                '("name" "key" "subscriber")) out))
      (setf *session-key* (getjso "key" session)))))

(defun get-session-key ()
  "Attempt to retrieve session key from disk. If it is not present, authorize
a new session with last.fm and store the key for future use."
  (if (probe-file (config-file "session"))
      (with-open-file (in (config-file "session")
                          :element-type '(unsigned-byte 8))
        (setf *session-key* (second (cl-store:restore in))))
      (authorize-scrobbling)))
