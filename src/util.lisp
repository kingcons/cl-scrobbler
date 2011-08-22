(in-package :cl-scrobbler)


;;;; General Utilities

;; Oh, On Lisp. You do have some fun macrology tools...
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun ksymb (&rest args)
  (values (intern (apply #'mkstr args) :keyword)))

;; This code happily stolen and adapted from Paktahn. Thanks Leslie!
(defun simplified-camel-case-to-lisp (camel-string)
  "Lispify camelCase or CamelCase strings to camel-case."
  (declare (string camel-string))
  (with-output-to-string (result)
    (loop for c across camel-string
       with last-was-lowercase
       when (and last-was-lowercase
                 (upper-case-p c))
       do (princ "-" result)
       if (lower-case-p c)
       do (setf last-was-lowercase t)
       else
       do (setf last-was-lowercase nil)
       do (princ (char-upcase c) result))))

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


;;;; Last.fm-specific utilities, macrology, etc

(defun frob-method-name (string)
  "Lispify Last.fm method names. i.e. auth.getToken -> get-token"
  ;; Note that if we intended to support the entire Last.fm API
  ;; discarding the namespace in this fashion would be undesirable,
  ;; unless we intended to use CLOS and EQL-specialized methods or similar.
  (let* ((namespace (position #\. string))
         (name (and namespace (subseq string (1+ namespace)))))
    (unless name
      (error "Couldn't parse name. Are you sure it's a valid Last.fm call?"))
    (symb (simplified-camel-case-to-lisp name))))

(defun make-signature (params)
  "Construct an API method signature from PARAMS."
  (let ((ordered (loop for (name . value) in (sort params #'string< :key #'car)
                    collecting (concatenate 'string name value))))
    (md5sum (apply #'concatenate 'string (append ordered `(,*api-secret*))))))

(defun lastfm-call (params &key (type :api) (method :get))
  "Make an HTTP request to the URL denoted by TYPE with the specified METHOD
and PARAMS. PARAMS should be a list of dotted pairs."
  (let ((base-uri (ecase type
                    (:api *api-url*)
                    (:submission *submission-url*))))
    (drakma:http-request base-uri :method method
                         :parameters (append '(("format" . "json")) params))))

(defmacro defcall (name params
                   (&key auth docs (type :api) (method :get))
                   &body body)
  "Define a function named by (FROB-METHOD-NAME NAME) which calls the API method
named by NAME with the given PARAMS. The result is bound to RESPONSE, the HTTP
status code to STATUS and the headers to HEADERS, then BODY is executed in this
context. Note that this macro is thus unhygienic. AUTH determines whether a
session key is also sent. DOCS is used to supply a docstring and TYPE and METHOD
are passed on to lastfm-call to modify the request URI and HTTP method."
  (let ((fn-label (frob-method-name name))
        (sig (gensym))
        (defaults `(("api_key" . ,*api-key*)
                    ("method" . ,name)
                    ,@(when auth `(("sk" . ,*session-key*))))))
    `(defun ,fn-label ,params
       ,@(when docs (list docs))
       (let* ((params ,(if params
                           `(append ,defaults ,@params)
                           `,defaults))
              (,sig (make-signature params)))
         (multiple-value-bind (response status headers)
             (lastfm-call (append params `(("api_sig" . ,,sig)))
                          :method ,method :type ,type)
           (let ((result ,@body))
             (values result status headers response)))))))
