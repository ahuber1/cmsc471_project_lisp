(setf *SUPPRESS-SIMILAR-CONSTANT-REDEITION-WARNING* t)

(defpackage :ANDREW-HUBER)
(in-package :ANDREW-HUBER)

(defclass my-game ()
	(
		;; PUBLIC -- list of players (each is an instance of type PLAYER)
		(players :accessor my-game-players :initarg :players)

		;; PUBLIC -- list of players who have been eliminated from the game
		(eliminated :accessor my-game-eliminated :initform NIL :initarg :eliminated)

		;; PUBLIC -- how many rounds (one turn per player) have been played
		(rounds :accessor my-game-rounds :initform 0 :initarg :rounds)

		(parent :accessor my-game-parent :initform nil :initarg :parent)	

		(step-stack :accessor my-game-step-stack :initform (make-instance 'my-stack) :initarg :step-stack)	

		(backup-stack :accessor my-game-backup-stack :initform nil :initarg :backup-stack)

		(current-player :accessor my-game-current-player :initform 0 :initarg :current-player)
	)
)

(defmethod current-player-object (game)
	(print "current-player-object")
	(nth (my-game-current-player game) (my-game-players game)))

(defun looses (player game)
	(print "looses")
	(setf (slot-value player 'coins) 0)
	(setf (slot-value game 'eliminated) (append (my-game-eliminated game) player)))

; are these copied correctly?
(defun convert-game (game player)
	(print "convert-game")
	(setf obj (make-instance 'my-game :players (copy-players game) :eliminated (coup::game-eliminated game) :rounds (coup::game-rounds game)))
	(setf (slot-value obj 'current-player) (index-of (coup::game-players game) player))
	obj)

(defun copy-players (origgame)
	(print "copy-players")
	(copy-players-aux (coup::game-players origgame) nil))

(defun copy-players-aux (players lst)
	(print "copy-players-aux")
	(if (null players)
		lst
		(progn
			(setf lst (append lst (list (make-instance 'coup::player :name (coup::player-name (car players)) :hand (coup::player-hand (car players)) :faceup (coup::player-faceup (car players)) 
				:exchange (coup::player-exchange (car players)) :handcount (coup::player-handcount (car players)) :numrounds (coup::player-numrounds (car players))
				:coins (coup::player-coins (car players)) :crashed (coup::player-crashed (car players))))))
			(copy-players-aux (cdr players) lst))))

(defun restore-step-stack (game)
	(print "restore-step-stack")
	(setf (slot-value game 'step-stack) nil)
	(setf (slot-value game 'step-stack) (copy-stack (slot-value game 'backup-stack))))

(defun depth (game)
	(print "depth")
	(depth-aux game 0))

(defun depth-aux (game depthVal)
	(print "depth-aux")
	(if (null game) 
		depthVal 
		(depth-aux (my-game-parent game) (+ depthVal 1))))

(defun winner (game)
	(print "winner")
	(setf counter 0)
	(setf winner nil)
	(dolist (player (my-game-players game))
		(if (not (lost player))
			(progn
				(setf winner player)
				(setf counter (+ counter 1)))))
	(if (eq counter 1) winner nil))

(defun players-equal (player1 player2)
	(print "players-equal")
	(if (or (null player1) (null player2))
		nil
		(string= (coup::player-name player1) (coup::player-name player2))))

(defun copy-game (game)
	(print "copy-game")
	(make-instance 'my-game :players (my-game-players game) :eliminated (my-game-eliminated game) 
		:rounds (my-game-rounds game) :parent (my-game-parent game) :step-stack (my-game-step-stack game) 
		:backup-stack (my-game-backup-stack game)))

(defun backup-stack (game) 
	(print "backup-stack")
	(setf (slot-value game 'backup-stack) (copy-stack (my-game-step-stack game))))

(defun increment-player (game)
	(print "increment-player")
	(setf (slot-value game 'current-player) (+ (my-game-current-player game) 1))
	(if (eq (length (my-game-players game)) (my-game-current-player game))
		(setf (slot-value game 'current-player) 0)))

(defun give-coins-to-all-players (game num-coins)
	(print "give-coins-to-all-players")
	(dolist (player (my-game-players game))
		(if (and (> (list-length (my-game-eliminated game)) 0) (not (member (my-game-eliminated game) player))) 
			(setf (slot-value player 'coup::player-coins) (+ (coup::player-coins player) num-coins)))))

(defun calculate-heuristic (game inquirer original-game)
	(print "calculate-heuristic")
	(setf x 0)
	(dolist (player (my-game-players game))
		(setf coins-gained (- (player-coins player) (player-coins (find-player original-game player))))
		(setf cards-lost (- (hand-count (find-player original-game player)) (hand-count player)))
		(if (players-equal player inquirer) 
			(setf x (+ x cards-lost)) 
			(setf x (+ x(* (* (list-length (my-game-players game)) (list-length (my-game-players game))) cards-lost)))))
	x)

(defun get-other-players-except (game player)
	(print "get-other-players-except")
	(get-other-players-except-aux player (my-game-players game) (list)))

(defun get-other-players-except-aux (player players players-found)
	(print "get-other-players-except-aux")
	(if (null players)
		(progn
			players-found)
		(progn
			(if (players-equal player (car players))
				(setf players-found (append players-found (list (car players)))))
			(get-other-players-except-aux player (cdr players) players-found))))

(defun lost (player)
	(print "lost")
	(eq (coup::player-handcount player) 0))

(defun clear-stacks (game)
	(print "clear-stacks")
	(empty-stack (my-game-step-stack))
	(empty-stack (my-game-backup-stack)))

(defun root (game)
	(print "root")
	(if (null (my-game-parent game))
		() ; do nothing
		(while (not (null (my-game-parent (my-game-parent (game))))) do
			(setf game (my-game-parent game))))
	game)


(defun find-player (game player)
	(print "find-player")
	(find-player-aux game player (my-game-player game)))

(defun find-player-aux (game player players)
	(print "find-player-aux")
	(if (null players)
		nil
		(if (players-equal (coup::player-name (car players)) player)
			(car players)
			(find-player-aux game player (cdr players)))))






