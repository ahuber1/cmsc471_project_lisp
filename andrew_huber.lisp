(setf *SUPPRESS-SIMILAR-CONSTANT-REDEFINITION-WARNING* t)

(defpackage :andrew_huber)
(in-package :andrew_huber)

(defun perform-move (player game)
	(setq queue (cons nil game))
		(setq g1 (perform-move-aux queue game player))
		(restore-step-stack g1)
		(xth-last-item-of-stack (step-stack g1) 1))

