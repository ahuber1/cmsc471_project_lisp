(defun theorize (effect instigator victim player cards-to-exchange game)
	(cond
		((eq effect 'Block) (theorize-block effect instigator victim player cards-to-exchange game num-coins lst))
		((eq effect 'Challenge) (theorize-challenge effect instigator victim player cards-to-exchange game num-coins lst))
		((eq effect 'Assassinate) (theorize-assassinate effect instigator victim player cards-to-exchange game num-coins lst))
		((eq effect 'Coup) (theorize-coup effect instigator victim player cards-to-exchange game num-coins lst))
		((eq effect 'Exchange) (theorize-exchange effect instigator victim player cards-to-exchange game num-coins lst))
		((eq effect 'ForeignAid) (theorize-foreign-aid effect instigator victim player cards-to-exchange game num-coins lst))
		((eq effect 'Income) (theorize-income effect instigator victim player cards-to-exchange game num-coins lst))
		((eq effect 'Steal) (theorize-steal effect instigator victim player cards-to-exchange game num-coins lst))
		((eq effect 'Tax) (theorize-tax effect instigator victim player cards-to-exchange game num-coins lst))
		(T error "Invalid effect")))

(defun theorize-block (effect instigator victim player cards-to-exchange game)
	(setq lst nil)
	(setq game-copy (copy-game game))
	(push-to-stack (my-game-step-stack game-copy) (makeinstance 'my-step :effect effect :instigator instigator :victim victim :ai player :cards cards-to-exchange))
	(append lst (theorize-challenge 'Block victim instigator instigator ai cards-to-challenge game-copy))
	lst)

(defun theorize-challenge (effect instigator victim player cards-to-exchange game)
	(setq lst nil)
	(if (not (null (my-step-get-cards (my-step-effect (my-stack-peek (my-game-step-stack game))))))
		(if (players-equal player (nth (my-game-players game) (my-game-current-player game)))
			(progn
				(setq game-copy (game-copy game))
				(my-stack-push (my-game-step-stack game-copy) (make-instance 'my-step :effect effect :instigator instigator :victim victim :ai player :cards cards-to-exchange))
				(append lst game-copy)))
		(progn
			(setq copy (game-copy game))
			(my-stack-push (my-game-step-stack copy) (make-instance 'my-step :effect effect :instigator instigator :victim victim :ai player :cards cards-to-exchange))
			(append lst copy)))
	(setq copy (game-copy game))
	(append lst game)
	lst)

(defun theorize-assassinate (effect instigator victim player cards-to-exchange game num-coins)
	(theorize-assassinate-generic effect instigator victim player cards-to-exchange game 3))

(defun theorize-assassinate-generic (effect instigator victim player cards-to-exchange game num-coins)
	(setq lst nil)
	(if (>= (player-coins instigator) num-coins)
		(progn
			(setq other-players (get-other-players-except game instigator))
			(dolist (other-player other-players)
				(setq possible-cards-to-assasinate nil)
				(if (or (eq cards-to-exchange nil) (> (list-length cards-to-exchange) 1)) 
					(setq possible-cards-to-assassinate (get-possible-cards-to-assassinate game player))
					(setq possible-cards-to-assassinate cards-to-exchange))
				(dolist (possible-card possible-cards-to-assassinate)
					(setq cards-to-exchange (list possible-card))
					(append lst (theorize-action parent instigator other-player player cards-to-exchange game lst))))))
	lst)

(defun theorize-coup (effect instigator victim player cards-to-exchange game)
	(theorize-assassinate-generic effect instigator victim player cards-to-exchange 7))

(defun theorize-exchange (effect instigator victim player cards-to-exchange game)
	(setq lst nil)
	(setq other-players (get-other-players-except game instigator))
	(if (eq player-handcount instigator 2)
		(dolist (other-player other-players)
			(dolist (card1 coup::Characters)
				(dolist (card2 coup::Characters)
					(setq cards-to-exchange (list card1 card2))
					(append lst (theorize-action effect instigator other-player player cards-to-exchange game))))))
	lst)

(defun theorize-foreign-aid (effect instigator victim player cards-to-exchange game)
	(setq other-players (get-other-players-except game instigator))
	(setq lst nil)
	(dolist (other-player other-players)
		(if (not (lost player))
			(append lst (theorize-action effect instigator other-player player cards-to-challenge game))))
	lst)

(defun theorize-income (effect instigator victim player cards-to-exchange game)
	(theorize-foreign-aid effect instigator victim player cards-to-exchange game))

(defun theorize-steal (effect instigator victim player cards-to-exchange game)
	(setq lst nil)
	(setq other-players (get-other-players-except game instigator))
	(dolist (other-player other-players)
		(if (>= (player-coins other-player) 2)
			(append lst (theorize-action effect instigator other-player player cards-to-exchange game))))
	lst)

(defun theorize-tax (effect instigator victim player cards-to-exchange game)
	(setq other-players (get-other-players-except game instigator))
	(setq lst nil)
	(dolist (other-player other-players)
		(if (not (lost player))
			(append lst (theorize-action effect instigator other-player player cards-to-challenge game))))
	lst)

(defun theorize-action (effect instigator victim player cards-to-exchange game)
	(if (is-action effect)
		(progn
			(setq lst nil)
			(setq counteractions (get-possible-blocks effect))
			(dolist (counter counteractions)
				(setq gameCopy (copy-game game))
				(my-stack-push (my-game-step-stack game) (make-instance 'my-step :parent effect :instigator instigator :victim victim :player player :cards-to-challenge cards-to-challenge))
				(append lst (theorize (get-counteraction counter) victim instigator player cards-to-exchange)))
			(if (null (get-card effect))
				(progn 
					(setq gameCopy (copy-game game))
					(my-stack-push (my-game-step-stack) (make-instance 'my-step :parent effect :instigator instigator :victim victim :player player :cards-to-challenge cards-to-challenge))
					(append lst (theorize nil victim instigator player cards-to-challenge gameCopy)))
				(progn
					(setq gameCopy (copy-game game))
					(my-stack-push (my-game-step-stack gameCopy) (make-instance 'my-step :parent effect :instigator instigator :victim victim :player player :cards-to-challenge cards-to-challenge))
					(append lst gameCopy))))
		(error "parent must be an action")))








