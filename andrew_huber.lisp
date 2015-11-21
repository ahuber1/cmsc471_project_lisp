(setf *SUPPRESS-SIMILAR-CONSTANT-REDEFINITION-WARNING* t)

(defpackage :ANDREW-HUBER)
(in-package :ANDREW-HUBER)

(load "my-game.lisp")
(load "my-step.lisp")
(load "my-stack.lisp")
(load "my-queue.lisp")
(load "coup.lisp")

(setq *depth* 0)
(setq *max-queue-size* 2000)
(setq *actions* (list 'Income 'ForeignAid 'Coup 'Duke 'Assassin 'Ambassador 'Captain 'Contessa))

(defun perform-move (player game)
	(setq queue (make-instnace 'my-queue))
	(setq g (convert-game game))
	(enqueue queue g)
	(setq g1 (perform-move-aux queue g player))
	(restore-step-stack g1)
	(xth-last-item-of-stack (step-stack g1) 1))

(defun perform-move-aux (queue game player)
	(while (not (null (my-queue-the-queue queue))) do
		(setq g (dequeue queue))
		(setq d (depth g))
		(if (and (not (null (winner g))) (players-equal (winner g) player))
			(progn
				(setq copy (copy-my-game g))
				(setq (slot-value copy 'my-game-parent) (slot-value g 'my-game-parent))
				(restore-step-stack copy)
				g)
			(if (and (not (null (winner g))) (not (players-equal (winner g) player)))
				() ; do nothing
				(if (and (not (eq d *depth*)) (>= (list-length q) *max-queue-size*))
					(perform-move-with-heuristic queue g game player)
					(progn
						(setq p (current-player g))
						(dolist (action *actions*)
							(setq games (theorize action nil p nil player nil g))
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
										(set-parent-game gm g)))))
						(dolist (card coup::Characters)
							(setq action (get-action card))
							(if (not (null action))
								(setq games (theorize action nil p nil player nil g))
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
	(loop while (not (null (my-queue-the-queue queue))) do
		(setq count (+ count 1))
		(setq heuristic-value (calculate-heuristic game player origGame))
		(setq lst (gethash 'heuristic-value games))
		(if (null lst)
			(setq (gethash 'heuristic-value games) '(game))
			(setq (gethash 'heuristic-value games) (append (gethash 'heuristic-value games) game)))
		(setq game (dequeue queue))
		(if (not (member heuristic-value keys))
			(append keys heuristic-value)))
	(setq keys (sort keys #'>))
	(car (gethash 'heuristic-value games)))

(defun reveal-card (player game)
	(setq queue (make-instance 'queue))
	(setq possible-cards-to-assassinate (get-possible-cards-to-assassinate game player))
	(dolist (possible-card possible-cards-to-assassinate)
		(setq cards-to-exchange '(possible-card))
		(enqueue queue (theorize (effect (peek (step-stack game))) nil (current-player game) player player cards-to-exchange game)))
	(setq g1 (perform-move queue game player))
	(if (not (null g1))
		(while (and (not (null g1)) (not (eq (parent-game g1) game))) do
			(setq g1 = (parent-game g1)))
		(if (not (null g1))
			(dolist (step (step-stack g1))
				(if (is-action (effect step))
					(+ (index-of (cards player) (cards-to-challenge step)))))))
	(if (> (cards player) 0)
		(+ (random (list-length (cards player))) 1)
		-1))

(defun get-possible-cards-to-assassinate (game player)
	(if (eq (current-player game) player)
		(cards player)
		coup::Characters))

(defun block-move (move player game source target)
	(if (is-action (effect move))
		(progn
			(setq action (effect move))
			(setq blocks (get-possible-blocks action))
			(setq lst nil)
			(setq queue (make-instance 'queue))
			(dolist (counter blocks)
				(setq game-copy (copy-my-game game))
				(clear-stacks game-copy)
				(append (step-stack game-copy) move)
				(append lst (theorize (counteraction counter) (counteraction counter) source (instigator move) player (cards-to-challenge move) game-copy)))
			(dolist (copy lst)
				(setq copy-of-copy (copy-my-game game))
				(parent-game copy game)
				(parent-game copy-of-copy copy)
				(backup-stack copy)
				(increment-player copy-of-copy)				
				(give-coins-to-all-players copy-of-copy 2)
				(clear-stacks copy-of-copy)
				(append queue copy-of-copy))
			(setq g1 (perform-move queue game player))
			(if (null g1)
				(null)
				(progn
					(setq g1 (root g1))
					(restore-step-stack g1)
					(xth-last-item-of-stack (step-stack g1) 2))))
		nil))

(defun challenge-card (card player game source target)
	(if (not (null card))
		(if (is-block (effect (peek (step-stack game))))
			(progn
				(setq block (effect (peek (step-stack game))))
				(setq lst nil)
				(setq queue (make-instance 'queue))
				(append lst (theorize challenge block player (instigator (peek (step-stack game))) player (cards-to-challenge (peek (step-stack game))) game))
				(dolist (copy lst)
					(setq copy-of-copy (copy-my-game game))
					(parent-game copy game)
					(parent-game copy-of-copy copy)
					(backup-stack copy)
					(increment-player copy-of-copy)				
					(give-coins-to-all-players copy-of-copy 2)
					(clear-stacks copy-of-copy)
					(enqueue queue copy-of-copy))
				(setq g1 (perform-move queue game player))
				(if (not (null g1))
					(if (is-challenge (effect (peek (step-stack g1))))
						(T)
						(nil))
					(nil)))
			(if (is-challenge (effect (peek (step-stack game))))
				(nil)
				(progn
					(setq lst nil)
					(setq q nil)
					(setq game-copy (copy-my-game game))
					(append lst (theorize challenge nil source (victim (peek (step-stack game))) player (cards-to-challenge (peek (step-stack game))) game-copy))
					(dolist (copy list)
						(setq copy-of-copy (copy-my-game game))
						(parent-game copy game)
						(parent-game copy-of-copy copy)
						(backup-stack copy)
						(increment-player copy-of-copy)				
						(give-coins-to-all-players copy-of-copy 2)
						(clear-stacks copy-of-copy)
						(enqueue queue copy-of-copy))
					(setq g1 (perform-move queue game player))
					(if (null g1)
						(nil)
						(progn
							(setq g1 (root g1))
							(restore-step-stack g1)
							(is-challenge (effect (xth-last-item-of-stack (step-stack g1) 2)))))))))
	nil)

(defun index-of (lst item)
	(index-of-aux lst item 0))

(defun index-of-aux (lst item index)
	(if (null lst)
		-1
		(if (eq item (car lst))
			index
			(index-of-aux (cdr lst) item (+ index 1)))))

(defun get-action (card)
	(cond 
		((eq card 'Duke) 'Tax)
		((eq card 'Assassin) 'Assassinate)
		((eq card 'Ambassador) 'Exchange)
		((eq card 'Captain) 'Steal)
		(T nil)))

(defun get-possible-blocks (action)
	(cond
		((eq action 'ForeignAid) (list 'Duke))
		((eq action 'Stealing) (list 'Ambassador 'Captain))
		((eq action 'Assassination) (list 'Contessa))
		(T nil)))

(defun get-possible-cards-to-assassinate (game player)
	(if (players-equal (nth (my-game-players game) (my-game-current-player game)) player)
		(copy-list (player-hand player)) 
		coup::Characters))

(defun is-block (block)
	(cond
		((eq block 'Duke) T)
		((eq block 'Ambassador) T)
		((eq block 'Captain) T)
		((eq block 'Contessa) T)
		(T nil)))

(defun is-challenge (challenge)
	(eq challenge 'Challenge))

(defun is-action (action)
	(cond
		((eq action 'Income) T)
		((eq action 'ForeignAid) T)
		((eq action 'Coup) T)
		((eq action 'Tax) T)
		((eq action 'Assassinate) T)
		((eq action 'Exchange) T)
		((eq action 'Steal) T)
		(T nil)))












