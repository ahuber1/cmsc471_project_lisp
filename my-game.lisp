(defclass my-game ()
	(
		;; PUBLIC -- list of players (each is an instance of type PLAYER)
		(players :accessor my-game-players :initarg :players)

		;; PUBLIC -- list of players who have been eliminated from the game
		(eliminated :accessor my-game-eliminated :initform NIL :initarg :eliminated)

		;; PUBLIC -- how many rounds (one turn per player) have been played
		(rounds :accessor my-game-rounds :initform 0 :initarg :rounds)

		(parent :accessor my-game-parent :initform nil :initarg :parent)	

		(step-stack :accessor my-game-step-stack :initform nil :initarg :step-stack)	

		(backup-stack :accessor my-game-backup-stack :initform nil :initarg :backup-stack)
	)
)

; are these copied correctly?
(defun convert-game (game)
	(make-instance 'my-game :players (game-players game) :eliminated (game-eliminated game) :rounds (game-rounds game)))