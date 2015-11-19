(defclass my-step ()
	(
		(effect :accessor step-effect :initarg effect)
		(instigator :accessor step-instigator :initarg instigator)
		(victim :accessor step-victim :initarg victim)
		(ai :accessor step-ai :initarg ai)
		(cards :accessor step-cards :initarg cards)
	)
)