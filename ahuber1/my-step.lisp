(setf *SUPPRESS-SIMILAR-CONSTANT-REDEITION-WARNING* t)

(defpackage :ANDREW-HUBER)
(in-package :ANDREW-HUBER)

(defclass my-step ()
	(
		(effect :accessor my-step-effect :initarg :effect)
		(instigator :accessor my-step-instigator :initarg :instigator)
		(victim :accessor my-step-victim :initarg :victim)
		(player :accessor my-step-player :initarg :player)
		(cards :accessor my-step-cards :initarg :cards)
	)
)