(defclass my-step ()
	(
		(effect :accessor my-step-effect :initarg effect)
		(instigator :accessor my-step-instigator :initarg instigator)
		(victim :accessor my-step-victim :initarg victim)
		(ai :accessor my-step-ai :initarg ai)
		(cards :accessor my-step-cards :initarg cards)
	)
)