(in-package :cl-scrobbler)


;;;; User Authentication - http://www.last.fm/api/authentication
;;;; For now, we only support Desktop Auth. http://www.last.fm/api/desktopauth

;;; Request Tokens

(defcall "auth.getToken" ()
  (:docs "Get an Unauthorized Token for initiating an authenticated session.")
  (getjso "token" (read-json response)))

(defun request-user-auth (token)
  "Ask the user to authorize the application."
  (format t "Please visit the following link in your browser: ~
http://www.last.fm/api/auth/?api_key=~a&token=~a" *api-key* token))

;;; Authenticated Sessions

(defcall "auth.getSession" (token)
  (:docs "Get a Session Token for scrobbling. Last.fm recommends storing these
in a secure fashion as they generally have an infinite lifetime."))

;;; Putting it all together...

(defun authorize-scrobbling ()
  (let ((token (get-token)))
    (request-user-auth token)
    (loop until (yes-or-no-p "Have you authorized cl-scrobbler?")
       do (request-user-auth token))
    (let ((session (getjso "session" (read-json (get-session token)))))
      (with-open-file (out (config-file "session") :direction :output
                           :if-exists :supersede)
        (loop for key in '("name" "key" "subscriber")
           do (write-line (getjso key session) out)))
      (setf *session-key* (getjso "key" session)))))
