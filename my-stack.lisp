(defclass my-stack ()
	(
		(the-stack :accessor my-stack-the-stack :initarg the-stack)
	)
)

(defun push-to-stack (stack item)
	(setq (slot-value stack 'the-stack) (append (list item) (my-stack-the-stack stack))))

(defun pop-to-stack (stack)
	(setq item (car (slot-value stack 'the-stack)))
	(setq (slot-value stack 'the-stack) (cdr (slot-value stack 'the-stack)))
	item)

(defun peek-from-stack (stack)
	(car (slot-value stack 'the-stack)))