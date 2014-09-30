;;;; orc.lisp

(in-package #:orc-battle)

;; orc
(defstruct (orc (:include monster)) (club-level (randval 8)))
(push #'make-orc *monster-builders*)

(defmethod monster-show ((m orc))
  (princa "A wicked orc with a level ")
  (princa (orc-club-level m))
  (princa " club"))

(defmethod monster-attack ((m orc))
  (let ((x (randval (orc-club-level m))))
    (princa "An orc swings his club at you and knocks off "
            x
            " of your health points. ")
    (fresh-line)
    (decf *player-health* x)))
