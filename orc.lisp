;;;; orc.lisp

(in-package #:orc-battle)

;; orc
(defstruct (orc (:include monster)) (club-level (randval 8)))
(push #'make-orc *monster-builders*)

(defmethod monster-show ((m orc))
  (print-on-sdl (format nil "A wicked orc with a level ~d club"
                        (orc-club-level m))))

(defmethod monster-attack ((m orc))
  (let ((x (randval (orc-club-level m))))
    (print-on-sdl (format nil "An orc swings his club at you and knocks off ~d of your health points." x))
    (decf *player-health* x)))
