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

(defun restore-step-stack (game)
	(setf (slot-value game 'step-stack) nil)
	(setf (slot-value game 'step-stack) (copy-stack (slot-value game 'backup-stack))))

(defun depth (game)
	(depth-aux game 0))

(defun depth (game depthVal)
	(if (null game) depthVal (depth (my-game-parent game) (+ depthVal 1))))

(defun winner (game)
	(setq counter 0)
	(setq winner nil)
	(dolist (player (my-game-players game))
		(if (not (lost player))
			(progn
				(setq winner player)
				(setq counter (+ counter 1)))))
	(if (eq counter 1) winner nil))

(defun players-equal (player1 player2)
	(eq (player-name player1) (player-name player2)))

(defun copy-game (game)
	(make-instance 'my-game :players (my-game-players game) :eliminated (my-game-eliminated game) 
		:rounds (my-game-rounds game) :parent (my-game-parent game) :step-stack (my-game-step-stack game) 
		:backup-stack (my-game-backup-stack game)))




