(setf *SUPPRESS-SIMILAR-CONSTANT-REDEITION-WARNING* t)

(defpackage :ANDREW-HUBER)
(in-package :ANDREW-HUBER)

(load "coup.lisp")
(load "ahuber1/my-game.lisp")
(load "ahuber1/my-step.lisp")
(load "ahuber1/my-stack.lisp")
(load "ahuber1/my-queue.lisp")
(load "ahuber1/theorize.lisp")
(load "ahuber1/execute.lisp")

(setq *depth* 0)
(setq *max-queue-size* 2000)

(defun perform-move (player game)
	(print "perform-move")
	(setq queue (make-instance 'my-queue))
	(setq g (convert-game game player))
	(enqueue queue g)
	(setq g1 (perform-move-aux queue g player))
	(restore-step-stack g1)
	(xth-last-item-of-stack (my-step-effect (my-game-step-stack g1) 1)))

(defun perform-move-aux (queue game player)
	(print "perform-move-aux")
	(loop while (not (null (my-queue-the-queue queue))) do
		(setq g (dequeue queue))
		(setq d (depth g))
		(if (and (not (null (winner g))) (players-equal (winner g) player))
			(progn
				(setq copy (copy-my-game g))
				(setq (slot-value copy 'my-game-parent) (slot-value g 'my-game-parent))
				(restore-step-stack copy)
				(return-from g))
			(if (and (not (null (winner g))) (not (players-equal (winner g) player)))
				() ; do nothing
				(if (and (not (eq d *depth*)) (>= (queue-size queue) *max-queue-size*))
					(return-from (perform-move-with-heuristic queue g game player))
					(progn
						(setq p (current-player-object g))
						(dolist (action coup::Moves)
							(setq games (theorize action p nil player nil g)) 
							(dolist (gm games)
								(setq c T)
								(dolist (my-game-step-stack gm)
									(if c
										(progn 
											(backup-stack gm)
											(setq step (pop (step-stack g)))
											(setq c (and c (execute (my-step-effect step) (my-step-instigator step) (my-step-victim step) player (my-step-cards step) game T))))))
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
									(dolist (my-game-step-stack gm)
										(if c
											(progn
												(backup-stack gm)
												(setq step (pop (step-stack g)))
												(setq c (and c (execute (effect step) (instigator step) (victim step) player (cards step) game T))))))
									(if c
										(progn
											(increment-player gm)
											(give-coins-to-all-players 2)
											(my-game-parent gm g)))))))))))
	nil)

(defun perform-move-with-heuristic (queue game origGame player)
	(print "perform-move-with-heuristic")
	(setq games (make-hash-table))
	(setq count 0)
	(setq keys nil)
	(loop while (not (queue-empty queue)) do
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
	(print "reveal-card")
	(setq queue (make-instance 'queue))
	(setq possible-cards-to-assassinate (get-possible-cards-to-assassinate game player))
	(dolist (possible-card possible-cards-to-assassinate)
		(setq cards '(possible-card))
		(enqueue queue (theorize (effect (peek-from-stack (my-game-step-stack game))) nil (current-player-object game) player player cards game)))
	(setq g1 (perform-move queue game player))
	(if (not (null g1))
		(loop while (and (not (null g1)) (not (eq (my-game-parent g1) game))) do
			(setq g1 = (my-game-parent g1)))
		(if (not (null g1))
			(dolist (step (my-game-step-stack g1))
				(if (is-action (my-step-effect step))
					(+ (index-of (cards player) (cards step)))))))
	(if (> (cards player) 0)
		(+ (random (list-length (cards player))) 1)
		-1))

(defun get-possible-cards-to-assassinate (game player)
	(print "get-possible-cards-to-assassinate")
	(if (players-equal (current-player-object game) player)
		(cards player)
		coup::Characters))

(defun block-move (move player game source target)
	(print "block-move")
	(if (is-action (my-step-effect move))
		(progn
			(setq action (effect move))
			(setq blocks (get-possible-blocks action))
			(setq lst nil)
			(setq queue (make-instance 'queue))
			(dolist (counter blocks)
				(setq copy (copy-my-game game))
				(clear-stacks copy)
				(push-to-stack (my-game-step-stack copy) move)
				(append lst (theorize (get-counteraction counter) source (my-step-instigator move) player (my-step-cards move) copy)
				(append lst (theorize (counteraction counter) (counteraction counter) source (instigator move) player (cards move) copy)))
			(dolist (copy lst)
				(setq copy-of-copy (copy-game game))
				(setq (slot-value copy 'parent) game)
				(setq (slot-value copy-of-copy 'parent) copy)
				(backup-stack copy)
				(increment-player copy-of-copy)				
				(give-coins-to-all-players copy-of-copy 2)
				(clear-stacks copy-of-copy)
				(enqueue queue copy-of-copy))
			(setq g1 (perform-move queue game player))
			(if (null g1)
				nil
				(progn
					(setq g1 (root g1))
					(restore-step-stack g1)
					(xth-last-item-of-stack (my-game-step-stack g1) 2))))
		nil)))

(defun challenge-card (card player game source target)
	(print "challenge-card")
	(if (not (null card))
		(if (is-block (my-step-effect (peek-from-stack (my-game-step-stack game))))
			(progn
				(setq block (effect (peek (step-stack game))))
				(setq lst nil)
				(setq queue (make-instance 'my-queue))
				(append lst (theorize 'Challenge block player (instigator (peek (step-stack game))) player (my-step-cards (peek-from-stack (my-game-step-stack game))) game))
				(dolist (copy lst)
					(setq copy-of-copy (copy-my-game game))
					(setq (slot-value copy 'parent) game)
					(setq (slot-value-copy-of-copy 'parent) copy)
					(backup-stack copy)
					(increment-player copy-of-copy)				
					(give-coins-to-all-players copy-of-copy 2)
					(clear-stacks copy-of-copy)
					(enqueue queue copy-of-copy))
				(setq g1 (perform-move queue game player))
				(if (not (null g1))
					(if (is-challenge (my-step-effect (peek-from-stack (my-game-step-stack g1))))
						T
						nil)
					nil)
			(if (is-challenge (effect (peek (step-stack game))))
				nil
				(progn
					(setq lst nil)
					(setq q nil)
					(setq game-copy (copy-my-game game))
					(append lst (theorize challenge nil source (my-step-victim (peek-from-stack (my-game-step-stack game))) player (my-step-cards (peek-from-stack (my-game-step-stack game))) game-copy))
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
					(if (null g1)
						nil
						(progn
							(setq g1 (root g1))
							(restore-step-stack g1)
							(is-challenge (my-step-effect (xth-last-item-of-stack (my-game-step-stack g1) 2))))))))))
	nil)

(defun index-of (lst item)
	(print "index-of")
	(index-of-aux lst item 0))

(defun index-of-aux (lst item index)
	(print "index-of-aux")
	(if (null lst)
		-1
		(if (eq item (car lst))
			index
			(index-of-aux (cdr lst) item (+ index 1)))))

(defun get-action (card)
	(print "get-action")
	(cond 
		((eq card 'Duke) 'Tax)
		((eq card 'Assassin) 'Assassinate)
		((eq card 'Ambassador) 'Exchange)
		((eq card 'Captain) 'Steal)
		(T nil)))

(defun get-counteraction (character)
	(print "get-counteraction")
	(cond
		((eq character 'Duke) 'BlocksForeignAid)
		((eq character 'Ambassador) 'BlocksStealing)
		((eq character 'Captain) 'BlocksStealing)
		((eq character 'Contessa) 'BlocksAssassination)))

(defun get-possible-blocks (action)
	(print "get-possible-blocks")
	(cond
		((eq action 'ForeignAid) (list 'Duke))
		((eq action 'Stealing) (list 'Ambassador 'Captain))
		((eq action 'Assassination) (list 'Contessa))
		(T nil)))

(defun get-possible-cards-to-assassinate (game player)
	(print "get-possible-cards-to-assassinate")
	(if (players-equal (current-player-object game) player)
		(copy-list (player-hand player)) 
		coup::Characters))

(defun is-block (block)
	(print "is-block")
	(cond
		((eq block 'Duke) T)
		((eq block 'Ambassador) T)
		((eq block 'Captain) T)
		((eq block 'Contessa) T)
		(T nil)))

(defun is-challenge (challenge)
	(print "is-challenge")
	(eq challenge 'Challenge))

(defun is-action (action)
	(print "is-action")
	(cond
		((eq action 'coup::Income) T)
		((eq action 'coup::ForeignAid) T)
		((eq action 'coup::Coup) T)
		((eq action 'coup::Tax) T)
		((eq action 'coup::Assassinate) T)
		((eq action 'coup::Exchange) T)
		((eq action 'coup::Steal) T)
		(T nil)))

(defun remove-card (game player card-num)
	(print "remove-card")
	(setq player (find-player game player))
	(setq (slot-value player 'hand) (remove-xth-item-from-list (player-hand player) card-num)))

(defun remove-xth-item-from-list (lst x)
	(print "remove-xth-item-from-list")
	(remove-xth-item-from-list-aux (lst nil x 1)))

(defun remove-xth-item-from-list-aux (origlist newlist x i)
	(print "remove-xth-item-from-list-aux")
	(if (null origlist)
		newlist
		(progn
			(if (not (eq x i)) 
				(append newlist (car origlist)))
			(remove-xth-item-from-list-aux (cdr origlist) newlist x (+ i 1)))))

(defun get-card (effect)
	(cond
		((eq effect 'coup::Income) nil)
		((eq effect 'coup::ForeignAid) nil)
		((eq effect 'coup::Coup) nil)
		((eq effect 'coup::Tax) 'coup::Duke)
		((eq effect 'coup::Assassinate) 'coup::Assassin)
		((eq effect 'coup::Exchange) 'coup::Ambassador)
		((eq effect 'coup::Steal) 'coup::Captain)
		(T (error "Invalid value"))))











