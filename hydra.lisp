;;;; hydra.lisp

(in-package #:orc-battle)

;; hydra
(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)

(defmethod monster-show ((m hydra))
  (princa "A malicious hydra with ")
  (princa (monster-health m))
  (princa " heads."))

(defmethod monster-hit ((m hydra) x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (progn (princa "The corpse of the fully decapitated and decapacitated hydra falls to the floor!")
             (fresh-line))
      (progn (princa "You lop off "
                     x
                     " of the hydra's heads! ")
             (fresh-line))))

(defmethod monster-attack ((m hydra))
  (let ((x (randval (ash (monster-health m) -1))))
    (princa "A hydra attacks you with "
            x
            " of its heads! It also grows back one more head! ")
    (fresh-line)
    (incf (monster-health m))
    (decf *player-health* x)))

