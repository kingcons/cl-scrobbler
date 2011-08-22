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
           do (write-line (getjso key session) out))))))


;;;; Submissions Protocol 1.2.1 - http://www.last.fm/api/submissions

;;; Handshake

(defun make-token (&key (type :web-service) (timestamp (unix-timestamp)))
  "Construct a token for web service or standard authentication. Defaults to
:web-service as :standard authentication may be deprecated in future versions."
  (ecase type
    (:web-service
     (md5sum (format nil "~a~d" *api-secret* timestamp)))
    (:standard
     (md5sum (format nil "~a~d" (md5sum *password*) timestamp)))))

(defun handshake ()
  (let* ((timestamp (unix-timestamp))
         (token (make-token :timestamp timestamp))
         (response (lastfm-call `(("hs" . "true")
                                  ("p" . ,*submission-protocol-version*)
                                  ("c" . ,*client-id*)
                                  ("v" . ,*client-version*)
                                  ("u" . ,*username*)
                                  ("t" . ,(format nil "~d" timestamp))
                                  ("a" . ,token)
                                  ("api_key" . ,*api-key*)
                                  ("sk" . ,*session-key*))
                                :type :submission)))
    response))


;;;; Submissions Protocol 2.0 - http://www.last.fm/api/scrobbling

;;; Handshake
