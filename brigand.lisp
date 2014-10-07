;;;; brigand.lisp

(in-package #:orc-battle)

;; brigand
(defstruct (brigand (:include monster)))
(push #'make-brigand *monster-builders*)

(defmethod monster-attack ((m brigand))
  (let ((x (max *player-health* *player-agility* *player-strength*)))
    (cond ((= x *player-health*)
           (print-on-sdl "A brigand hits you with his slingshot, taking off 2 health points! ")
           (fresh-line)
           (decf *player-health* 2))
          ((= x *player-agility*)
           (print-on-sdl "A brigand catches your leg with his whip, taking off 2 agility points! ")
           (fresh-line)
           (decf *player-agility* 2))
          ((= x *player-strength*)
           (print-on-sdl "A brigand cuts your arm with his whip, taking off 2 strength points!")
           (fresh-line)
           (decf *player-strength* 2)))))
