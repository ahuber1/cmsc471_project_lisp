(defclass my-queue ()
	(
		(the-queue :accessor my-queue-the-queue :initarg the-queue :initform nil)
	)
)

(defun enqueue (queue item)
	(setq (slot-value queue 'the-queue) (append (my-queue-the-queue queue) (list item) )))

(defun dequeue (stack)
	(setq item (car (slot-value queue 'the-queue)))
	(setq (slot-value queue 'the-queue) (cdr (slot-value queue 'the-queue)))
	item)

(defun peek-from-queue (queue)
	(car (slot-value queue 'the-queue)))

(defun queue-size (queue) 
	(list-length (my-queue-the-queue queue)))

(defun queue-empty (queue)
	(eq 0 (list-length (my-queue-the-queue queue))))