(setf *SUPPRESS-SIMILAR-CONSTANT-REDEITION-WARNING* t)

(defpackage :ANDREW-HUBER)
(in-package :ANDREW-HUBER)

(defclass my-stack ()
	(
		(the-stack :accessor my-stack-the-stack :initarg :the-stack :initform nil)
	)
)

(defun push-to-stack (stack item)
	(setf (slot-value stack 'the-stack) (append (list item) (my-stack-the-stack stack))))

(defun pop-from-stack (stack)
	(print "pop-from-stack")
	(setf item (car (slot-value stack 'the-stack)))
	(setf (slot-value stack 'the-stack) (cdr (slot-value stack 'the-stack)))
	item)

(defun peek-from-stack (stack)
	(print "peek-from-stack")
	(car (slot-value stack 'the-stack)))

(defun stack-size (stack) 
	(print "stack-size")
	(list-length (my-stack-the-stack stack)))

(defun is-empty (stack)
	(print "is-empty")
	(eq 0 (list-length (my-stack-the-stack stack))))

(defun xth-last-item-of-stack (stack x)
	(print "xth-last-item-of-stack")
	(setf copy (copy-stack stack))
	(loop while (> (stack-size copy) x) do
		(pop-from-stack copy))
	(pop-from-stack copy))

(defun empty-stack (stack)
	(print "empty-stack")
	(setf (slot-value stack 'the-stack) nil))

(defun copy-stack (stack)
	(print "copy-stack")
	(setf temp (make-instance 'my-stack))
	(setf new (make-instance 'my-stack))
	(loop while (not (is-empty stack)) do
		(push-to-stack temp (pop-from-stack stack)))
	(loop while (not (is-empty temp)) do
		(setf data (pop-from-stack temp))
		(push-to-stack stack data)
		(push-to-stack new data))
	new)
