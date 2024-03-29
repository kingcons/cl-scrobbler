(in-package :cl-scrobbler)


;;;; Last.fm error codes, conditions and logging

(defvar *error-codes*
  '((1 . "This error does not exist")
    (2 . "Invalid Service - This service does not exist")
    (3 . "Invalid Method - No method with that name in this package")
    (4 . "Authentication Failed - You do not have permission to access this service")
    (5 . "Invalid Format - This service doesn't exist in that format")
    (6 . "Invalid Parameters - Your request is missing a required paramter")
    (7 . "Invalid Resource specified")
    (8 . "Operation failed - Most likely the backend service failed. Please try again.")
    (9 . "Invalid Session Key - Please reauthenticate")
    (10 . "Invalid API Key - You must be granted a valid key by last.fm")
    (11 . "Service Offline - This service is temporarily offline. Try again later")
    (12 . "Subscribers Only - This station is only available to paid last.fm customers")
    (13 . "Invalid Method Signature supplied")
    (14 . "Unauthorized Token - This token has not been authorized")
    (15 . "This item is not available for streaming")
    (16 . "The service is temporarily unavailable, please try again")
    (17 . "Login - Requires the user to be logged in")
    (18 . "Trial Expired: The user has no free radio plays left. Subscription required")
    (19 . "This error does not exist")
    (20 . "Not Enough Content - There is not enough content to play this station")
    (21 . "Not Enough Members - This group does not have enough members for radio")
    (22 . "Not Enough Fans - This artist does not have enough fans for radio")
    (23 . "Not Enough Neighbours - There are not enough neighbours for radio")
    (24 . "No Peak Radio - This user is not allowed to listen to radio during peak usage")
    (25 . "Radio Not Found - Radio station not found")
    (26 . "API Key Suspended - This application is not allowed to make requests to the web services")
    (27 . "Deprecated - This type of request is no longer supported")
    (29 . "Rate Limit Exceded - Your IP has made too many requests in a short period, exceeding our API guidelines"))
  "A list of Last.fm Error codes. Duh. From http://www.last.fm/api/errorcodes")

(defun error-message (errcode)
  "Retrieve the error message corresponding to the number ERRCODE."
  (rest (nth (1- errcode) *error-codes*)))

(defun add-log-entry (&rest args)
  "Format ARGS and append the line to the cl-scrobbler log file, creating it
if it does not exist."
  (with-open-file (out (config-file "scrobbler-errors.log") :direction :output
                       :if-does-not-exist :create :if-exists :append)
    (write-line (apply #'format nil args) out)))

(define-condition lastfm-server-error (error)
  ((message :initarg :message :reader message)))

(define-condition scrobble-error (error) ())

(defmacro with-logging (() &body body)
  "Execute BODY in a handler-case such that network failure or any error message
from the server results in logging the error to disk via ADD-LOG-ENTRY."
  `(multiple-value-bind (second minute hour day month year)
       (get-decoded-time)
     (let ((timestamp (format nil "~2,'0d/~2,'0d/~d -- ~2,'0d:~2,'0d:~2,'0d"
                              month day year hour minute second)))
       (handler-case (progn ,@body)
         (usocket:socket-error ()
           (add-log-entry "[~a]> Socket error connecting to last.fm."
                          timestamp))
         (usocket:ns-condition ()
           (add-log-entry "[~a]> DNS lookup error connecting to last.fm."
                          timestamp))
         (lastfm-server-error (e)
           (add-log-entry "[~a]> Last.fm Error: ~a"
                          timestamp (message e)))))))
