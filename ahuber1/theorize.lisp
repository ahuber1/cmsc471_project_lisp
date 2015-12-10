(setf *SUPPRESS-SIMILAR-CONSTANT-REDEITION-WARNING* t)

(defpackage :ANDREW-HUBER)
(in-package :ANDREW-HUBER)

(defun theorize (effect instigator victim player cards game)
	(cond
		((eq effect 'coup::Block) (theorize-block effect instigator victim player cards game))
		((eq effect 'coup::Challenge) (theorize-challenge effect instigator victim player cards gamelst))
		((eq effect 'coup::Assassinate) (theorize-assassinate effect instigator victim player cards game))
		((eq effect 'coup::Coup) (theorize-coup effect instigator victim player cards game))
		((eq effect 'coup::Exchange) (theorize-exchange effect instigator victim player cards game))
		((eq effect 'coup::ForeignAid) (theorize-foreign-aid effect instigator victim player cards game))
		((eq effect 'coup::Income) (theorize-income effect instigator victim player cards game))
		((eq effect 'coup::Steal) (theorize-steal effect instigator victim player cards game))
		((eq effect 'coup::Tax) (theorize-tax effect instigator victim player cards game))
		(T error "Invalid effect")))

(defun theorize-block (effect instigator victim player cards game)
	(setq lst nil)
	(setq game-copy (copy-game game))
	(push-to-stack (my-game-step-stack game-copy) (makeinstance 'my-step :effect effect :instigator instigator :victim victim :ai player :cards cards))
	(setq lst (append lst (theorize-challenge 'Block victim instigator instigator ai cards game-copy)))
	lst)

(defun theorize-challenge (effect instigator victim player cards game)
	(setq lst nil)
	(if (not (null (my-step-get-cards (my-step-effect (my-stack-peek (my-game-step-stack game))))))
		(if (players-equal player (current-player-object game))
			(progn
				(setq game-copy (game-copy game))
				(my-stack-push (my-game-step-stack game-copy) (make-instance 'my-step :effect effect :instigator instigator :victim victim :ai player :cards cards))
				(setq lst (append lst game-copy))))
		(progn
			(setq copy (game-copy game))
			(my-stack-push (my-game-step-stack copy) (make-instance 'my-step :effect effect :instigator instigator :victim victim :ai player :cards cards))
			(setq lst (append lst copy))))
	(setq copy (game-copy game))
	(setq lst (append lst game))
	lst)

(defun theorize-assassinate (effect instigator victim player cards game num-coins)
	(theorize-assassinate-generic effect instigator victim player cards game 3))

(defun theorize-assassinate-generic (effect instigator victim player cards game num-coins)
	(setq lst nil)
	(print "Instigator")
	(print instigator)
	(print (my-game-players game))
	(setq instigator (nth (my-game-players game) instigator))
	(print "Instigator")
	(print instigator)
	(if (>= (coup::player-coins instigator) num-coins)
		(progn
			(setq other-players (get-other-players-except game instigator))
			(dolist (other-player other-players)
				(setq possible-cards-to-assasinate nil)
				(if (or (eq cards nil) (> (list-length cards) 1)) 
					(setq possible-cards-to-assassinate (get-possible-cards-to-assassinate game player))
					(setq possible-cards-to-assassinate cards))
				(dolist (possible-card possible-cards-to-assassinate)
					(setq cards (list possible-card))
					(setq cards (append lst (theorize-action parent instigator other-player player cards game lst)))))))
	lst)

(defun theorize-coup (effect instigator victim player cards game)
	(theorize-assassinate-generic effect instigator victim player cards game 7))

(defun theorize-exchange (effect instigator victim player cards game)
	(setq lst nil)
	(setq other-players (get-other-players-except game instigator))
	(if (eq coup::player-handcount instigator 2)
		(dolist (other-player other-players)
			(dolist (card1 coup::Characters)
				(dolist (card2 coup::Characters)
					(setq cards (list card1 card2))
					(setq (append lst (theorize-action effect instigator other-player player cards game)))))))
	lst)

(defun theorize-foreign-aid (effect instigator victim player cards game)
	(setq other-players (get-other-players-except game instigator))
	(setq lst nil)
	(dolist (other-player other-players)
		(if (not (lost player))
			(setq lst (append lst (theorize-action effect instigator other-player player cards game)))))
	lst)

(defun theorize-income (effect instigator victim player cards game)
	(theorize-foreign-aid effect instigator victim player cards game))

(defun theorize-steal (effect instigator victim player cards game)
	(setq lst nil)
	(setq other-players (get-other-players-except game instigator))
	(dolist (other-player other-players)
		(if (>= (coup::player-coins other-player) 2)
			(setq lst (append lst (theorize-action effect instigator other-player player cards game)))))
	lst)

(defun theorize-tax (effect instigator victim player cards game)
	(setq other-players (get-other-players-except game instigator))
	(setq lst nil)
	(dolist (other-player other-players)
		(if (not (lost player))
			(setq lst (append lst (theorize-action effect instigator other-player player cards game)))))
	lst)

(defun theorize-action (effect instigator victim player cards game)
	(if (is-action effect)
		(progn
			(setq lst nil)
			(setq counteractions (get-possible-blocks effect))
			(dolist (counter counteractions)
				(setq gameCopy (copy-game game))
				(my-stack-push (my-game-step-stack game) (make-instance 'my-step :parent effect :instigator instigator :victim victim :player player :cards cards))
				(setq lst (append lst (theorize (get-counteraction counter) victim instigator player cards))))
			(if (null (get-card effect))
				(progn 
					(setq gameCopy (copy-game game))
					(my-stack-push (my-game-step-stack) (make-instance 'my-step :parent effect :instigator instigator :victim victim :player player :cards cards))
					(setq lst (append lst (theorize nil victim instigator player cards gameCopy))))
				(progn
					(setq gameCopy (copy-game game))
					(my-stack-push (my-game-step-stack gameCopy) (make-instance 'my-step :parent effect :instigator instigator :victim victim :player player :cards cards))
					(setq lst (append lst gameCopy)))))
		(error "parent must be an action")))








