(in-package :cl-scrobbler)


;;;; General Utilities

;; This code stolen and adapted from local-time. Hooray local-time!
(defun unix-timestamp ()
  "Cross-implementation abstraction to get the current time measured from the
unix epoch (1/1/1970). Should return (values sec nano-sec)."
  #+cmu
  (multiple-value-bind (success? sec usec) (unix:unix-gettimeofday)
    (assert success? () "unix:unix-gettimeofday reported failure?!")
    (values sec (* 1000 usec)))
  #+sbcl
  (multiple-value-bind (sec nsec) (sb-ext:get-time-of-day)
    (values sec (* 1000 nsec)))
  #+(and ccl (not windows))
  (ccl:rlet ((tv :timeval))
    (let ((err (ccl:external-call "gettimeofday" :address tv
                                  :address (ccl:%null-ptr) :int)))
      (assert (zerop err) nil "gettimeofday failed")
      (values (ccl:pref tv :timeval.tv_sec)
              (* 1000 (ccl:pref tv :timeval.tv_usec)))))
  #-(or cmu sbcl (and ccl (not windows)))
  (values (- (get-universal-time)
             ;; CL's get-universal-time uses an epoch of 1/1/1900. Use unix's.
             #.(encode-universal-time 0 0 0 1 1 1970 0))
          0))

(defun md5sum (string)
  "Creates an MD5 byte-array of STRING and prints it as lower-case hexadecimal."
  (format nil "~(~{~2,'0X~}~)"
          (map 'list #'identity (md5:md5sum-sequence string))))

(defun lastfm-call (params &key (type :api) (method :get))
  "Make an HTTP request to the URL denoted by TYPE with the specified METHOD
and PARAMS. PARAMS should be a list of dotted pairs."
  (let ((base-uri (ecase type
                    (:api *api-url*)
                    (:submission *submission-url*))))
    (drakma:http-request base-uri :method method
                         :parameters (append '(("format" . "json")) params))))


;;;; User Authentication - http://www.last.fm/api/authentication
;;;; For now, we only support Desktop Auth. http://www.last.fm/api/desktopauth

;;; Call signing

(defun make-signature (params)
  "Construct an API method signature from PARAMS."
  (let ((ordered (loop for (name . value) in (sort params #'string< :key #'car)
                    collecting (concatenate 'string name value))))
    (md5sum (apply #'concatenate 'string (append ordered `(,*api-secret*))))))

;;; Request Tokens

(defun get-unauthorized-token ()
  "Get an Unauthorized Token for use in initiating an authenticated session."
  (let* ((params `(("api_key" . ,*api-key*)
                   ("method" . "auth.getToken")))
         (sig (make-signature params))
         (response (lastfm-call (append params `(("api_sig" . ,sig))))))
    (getjso "token" (read-json response))))

(defun request-user-auth (token)
  "Ask the user to authorize the application."
  (format t "Please visit the following link in your browser: ~
http://www.last.fm/api/auth/?api_key=~a&token=~a" *api-key* token))

;;; Authenticated Sessions

(defun get-session (token)
  (let* ((params `(("api_key" . ,*api-key*)
                   ("method" . "auth.getSession")
                   ("token" . ,token)))
         (sig (make-signature params)))
    (lastfm-call (append params `(("api_sig" . ,sig))))))

;;; Putting it all together...

(defun authorize-scrobbling ()
  (let ((token (get-unauthorized-token)))
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
         (response (lastfm-call `(("hs" "true")
                                  ("p" ,*submission-protocol-version*)
                                  ("c" ,*client-id*)
                                  ("v" ,*client-version*)
                                  ("u" ,*username*)
                                  ("t" ,timestamp)
                                  ("a" ,token)
                                  ("api_key" ,*api-key*)
                                  ("sk" ,*session-key*))
                                :type :submission)))
    ;; TODO!
    ))


;;;; SCRATCH

(defun encode-args (args)
  "Construct a querystring from ARGS which should be a list of pairs."
  (string-downcase (format nil "?~{~{~a~^=~}~^&~}" args)))
