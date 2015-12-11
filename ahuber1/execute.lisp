(setf *SUPPRESS-SIMILAR-CONSTANT-REDEITION-WARNING* t)

(defpackage :ANDREW-HUBER)
(in-package :ANDREW-HUBER)

(defun execute (instigator victim player cards game theorizing effect)
	(print "execute")
	(cond
		((eq effect 'Block) (execute-block instigator victim player cards game theorizing))
		((eq effect 'Challenge) (execute-challenge instigator victim player cards game theorizing))
		((eq effect 'Assassinate) (execute-assassination instigator victim player cards game theorizing))
		((eq effect 'Coup) (execute-coup instigator victim player cards game theorizing))
		((eq effect 'Exchange) (execute-exchange victim player cards game theorizing))
		((eq effect 'ForeignAid) (execute-foreign-aid victim player cards game theorizing))
		((eq effect 'Income) (execute-income victim player cards game theorizing))
		((eq effect 'Steal) (execute-steal victim player cards game theorizing))
		((eq effect 'Tax) (execute-tax victim player cards game theorizing))
		(T)))

(defun execute-block (instigator victim player cards game theorizing)
	(print "execute-block")
	(empty-stack (my-game-step-stack game))
	T)

(defun execute-challenge (instigator victim player cards game theorizing)
	(print "execute-challenge")
	(setf instigator (find-player game instigator))
	(setf victim (find-player game victim))
	(setf temp (find-player game player))
	(if (theorizing)
		(looses player game)
		(error "execute-challenge ... not theorizing")))

(defun execute-assassinate (instigator victim player cards game theorizing)
	(print "execute-assassinate")
	(execute-assassinate-or-coup instigator victim player cards game theorizing 3))

(defun execute-coup (instigator victim player cards game theorizing)
	(print "execute-coup")
	(execute-assassinate-or-coup instigator victim player cards game theorizing 7))

(defun execute-assassinate-or-coup (instigator victim player cards game theorizing num-coins)
	(print "execute-assassinate-or-coup")
	(setf instigator (find-player game instigator))
	(setf victim (find-player game instigator))
	(setf temp (find-player game player))
	(if (>= (player-coins instigator) num-coins)
		(progn
			(setf (slot-value instigator 'coins) (- (player-coins instigator) num-coins))
			(setf is-victim-agent (players-equal victim player))
			(if (and (is-victim-agent (not (theorizing))))
				(setf cardnum (reveal-card victim game))
				(if (> index 0)
					(remove-card game player index)
					nil)
				(if (is-victim-agent)
					(member (player-hand victim) (car cards))
					(if (> (list-length (player-hand victim)) 0)
						(progn 
							(remove-card game victim 1)
							T)
						nil))))
		nil))

(defun execute-exchange (instigator victim player cards game theorizing) 
	(print "execute-exchange")
	nil) ; I cannot access deck; assume this can't be done; return nil

(defun execute-foreign-exchange (instigator victim player cards game theorizing)
	(print "execute-foreign-exchange")
	(execute-foreign-aid-or-income instigator victim player cards game theorizing 2))

(defun execute-income (instigator victim player cards game theorizing)
	(print "execute-income")
	(execute-foreign-aid-or-income instigator victim player cards game theorizing 1))

(defun execute-foreign-aid-or-income (instigator victim player cards game theorizing num-coins)
	(print "execute-foreign-aid-or-income")
	(setf instigator (find-player game instigator))
	(setf (slot-value instigator 'coins) (+ (player-coins instigator) num-coins))
	T)

(defun execute-steal (instigator victim player cards game theorizing)
	(print "execute-steal")
	(setf instigator (find-player game instigator))
	(setf victim (find-player game victim))
	(if (>= (player-coins victim) 2)
		(progn
			(setf (slot-value victim 'coins) (- (player-coins victim) 2))
			(setf (slot-value instigator 'coins) (+ (player-coins instigator) 2))
			T)
		nil))

(defun execute-tax (instigator victim player cards game theorizing)
	(print "execute-tax")
	(setf instigator (find-player game instigator))
	(setf (slot-value instigator 'coins) (+ (player-coins instigator) 3))
	T)







