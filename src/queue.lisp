(in-package :cl-scrobbler)


;;;; This queue code courtesy of Peter Norvig and Richard Water's technical
;;;; paper "Implementing Queues in Lisp" published by the ACM in 1991.
;;;; It is more easily, and freely, available in 'Some Useful Lisp Algorithms'
;;;; which is available online at http://www.merl.com/papers/docs/TR91-04.pdf

;;; This implementation taken from Figure 17 and ever so slightly adapted.
;;; TODO: Read the paper. Then scrap the CDR coding. It's 2011 after all.

(defun make-queue ()
  (let ((queue (list nil)))
    (cons queue queue)))

(defun queue-elements (queue)
  (cdar queue))

(defun empty-queue-p (queue)
  (null (queue-elements queue)))

(defun queue-front (queue)
  (cadar queue))

(defun dequeue (queue)
  (car (setf (car queue) (queue-elements queue))))

(defun enqueue (queue item)
  (setf (cdr queue)
        (setf (cddr queue) (list item))))
