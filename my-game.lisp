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

		(current-player :accessor my-game-current-player :initform 0)
	)
)

; are these copied correctly?
(defun convert-game (game)
	(make-instance 'my-game :players (copy-players (game-players game)) :eliminated (game-eliminated game) :rounds (game-rounds game)))

(defun restore-step-stack (game)
	(setf (slot-value game 'step-stack) nil)
	(setf (slot-value game 'step-stack) (copy-stack (slot-value game 'backup-stack))))

(defun depth (game)
	(depth-aux game 0))

(defun depth-aux (game depthVal)
	(if (null game) depthVal (depth-aux (my-game-parent game) (+ depthVal 1))))

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

(defun backup-stack (game) (backup-stack-aux (my-game-step-stack game)))

(defun backup-stack-aux (backup-stack)
	(if (eq 0 (length (my-stack-the-stack backup-stack)))
		(backup-stack)
		(progn
			(push-to-stack backup-stack)
			(backup-stack-aux backup-stack))))

(defun increment-player (game)
	(setf (slot-value game 'current-player) (+ (my-game-current-player game) 1))
	(if (eq (length (my-game-players game)) (my-game-current-player game))
		(setf (slot-value game 'current-player) 0)))

(defun give-coins-to-all-players (game num-coins)
	(dolist (player (my-game-players game))
		(if (not (member (my-game-eliminated game) player)) 
			(setq (slot-value player 'player-coins) (+ (player-coins player) num-coins)))))

(defun calculate-heuristic (game inquirer original-game)
	(setq x 0)
	(dolist (player (my-game-players game))
		(setq coins-gained (- (player-coins player) (player-coins (find-player original-game player))))
		(setq cards-lost (- (hand-count (find-player original-game player)) (hand-count player)))
		(if (players-equal player inquirer) 
			(setq x (+ x cards-lost)) 
			(setq x (+ x(* (* (list-length (my-game-players game)) (list-length (my-game-players game))) cards-lost)))))
	x)








