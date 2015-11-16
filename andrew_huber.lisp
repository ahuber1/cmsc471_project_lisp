(setf *SUPPRESS-SIMILAR-CONSTANT-REDEFINITION-WARNING* t)

(defpackage :andrew_huber)
(in-package :andrew_huber)

(defclass my-game ()
	(
		;; PUBLIC -- list of players (each is an instance of type PLAYER)
		(players :accessor my-game-players :initarg :players)

		;; PUBLIC -- list of players who have been eliminated from the game
		(eliminated :accessor my-game-eliminated :initform NIL :initarg :eliminated)

		;; PUBLIC -- how many rounds (one turn per player) have been played
		(rounds :accessor my-game-rounds :initform 0 :initarg :rounds)

		(parent :accessor my-game-parent :initform nil :initarg :accessor)		
	)
)


(defun perform-move (player game)
	(setq queue (cons nil game))
		(setq g (convert-game game))
		(setq g1 (perform-move-aux queue g player))
		(restore-step-stack g1)
		(xth-last-item-of-stack (step-stack g1) 1))

(defun perform-move-aux (queue game player)
	(dolist (g queue)
		(setq g (car queue))
		(setq d (depth g))
		(if (and (not (null (winner g))) (players-equal (winner g) player))
			(progn
				(setq copy (copy-my-game g))
				(setq (slot-value copy 'my-game-parent))
				(restore-step-stack copy)
				g)
			(if (and (not (null (winner g))) (not (players-equal (winner g) player)))
				() ; do nothing
				(if (and (not (eq d *depth*)) (>= (list-length q) *max-queue-size*))
					(perform-move-with-heuristic queue g game player)
					(progn
						(setq p (current-player g))
						(dolist (action *actions*)
							(setq games (theorize action))
							(dolist (gm games)
								(setq c T)
								(dolist (step-stack gm)
									(if c
										(progn 
											(backup-stack gm)
											(setq step (pop (step-stack g)))
											(setq c (and c (execute (effect step) (instigator step) (victim step) player (cards-to-challenge step) game T))))))
								(if c
									(progn
										(increment-player gm)
										(give-coins-to-all-players 2)
										(parent-game gm g)))))
						(dolist (card *cards*)
							(setq action (get-action card))
							(if (not (null action))
								(setq games (theorize action))
								(dolist (gm games)
									(setq c T)
									(dolist (step-stack gm)
										(if c
											(progn
												(backup-stack gm)
												(setq step (pop (step-stack g)))
												(setq c (and c (execute (effect step) (instigator step) (victim step) player (cards-to-challenge step) game T))))))
									(if c
										(progn
											(increment-player gm)
											(give-coins-to-all-players 2)
											(parent-game gm g)))))))))))
	nil)

(defun perform-move-with-heuristic (queue game origGame player)
	(setq games (make-hash-table))
	(setq count 0)
	(setq keys nil)
	(loop while (not (null queue)) do
		(setq count (+ count 1))
		(setq heuristic-value (calculate-heuristic game player origGame))
		(setq lst (gethash 'heuristic-value games))
		(if (null lst)
			(setq (gethash 'heuristic-value games) '(game))
			(setq (gethash 'heuristic-value games) (append (gethash 'heuristic-value games) game)))
		(setq queue (cdr queue))
		(setq game (car queue))
		(if (not (member heuristic-value keys))
			(append keys heuristic-value)))
	(setq keys (sort keys #'>))
	(car (gethash 'heuristic-value games)))

