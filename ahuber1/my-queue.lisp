(setf *SUPPRESS-SIMILAR-CONSTANT-REDEITION-WARNING* t)

(defpackage :ANDREW-HUBER)
(in-package :ANDREW-HUBER)

(defclass my-queue ()
	(
		(the-queue :accessor my-queue-the-queue :initarg :the-queue :initform nil)
	)
)

(defun enqueue (queue item)
	(print "enqueue")
	(setf (slot-value queue 'the-queue) (append (my-queue-the-queue queue) (list item))))

(defun dequeue (queue)
	(print "dequeue")
	(setf item (car (slot-value queue 'the-queue)))
	(setf (slot-value queue 'the-queue) (cdr (slot-value queue 'the-queue)))
	item)

(defun peek-from-queue (queue)
	(print "peek-from-queue")
	(car (slot-value queue 'the-queue)))

(defun queue-size (queue) 
	(print "queue-size")
	(list-length (my-queue-the-queue queue)))

(defun queue-empty (queue)
	(print "queue-empty")
	(eq 0 (list-length (my-queue-the-queue queue))))