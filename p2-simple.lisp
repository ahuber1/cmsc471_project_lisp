;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; p2-simple.lisp - default (and rather dumb) Coup player for
;; CMSC471 project. Will select relatively arbitrary actions.
;; Coups if 7 coins available, will otherwise select a random action
;;
;; (c) Shawn Squire 2015
;; Version 1.0 - Distributed 10/6/2015
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PACKAGE DEFINITION
;; You MUST include these two lines, with YOUR OWN TEAM NAME
;; for the defined package.  All of your code "lives" in this
;; package, and it must be the same as the team name that you
;; will use for the tournament!!

(defpackage :p2)
(in-package :p2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; IMPORTS - symbols (function names) to import from the main
;; game engine package.  Add imports of utility functions here
;; if you choose to use any.
;;

(import '(coup::Moves
					coup::Characters
					coup::CardsPerCharacter coup::CardsPerPlayer
					coup::game-players coup::game-rounds
					coup::player-name coup::player-hand coup::player-faceup
					coup::player-exchange coup::player-handcount
					coup::player-numrounds coup::player-coins
					coup::card-move coup::card-block coup::move-card coup::block-cards
					))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; REQUIRED FUNCTIONS

(defun perform-move (player game)
	(if (< (player-coins player) 7)
		'(coup::Income)
		'(coup::Coup))
)

(defun reveal-card (player game)
	2)

(defun select-exchange (player game)
	'((1 . 1) (2 . 2)))

(defun block-move (move player game source &optional target))
(defun challenge-card (card player game source &optional target))
