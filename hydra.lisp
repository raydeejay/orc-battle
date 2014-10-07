;;;; hydra.lisp

(in-package #:orc-battle)

;; hydra
(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)

(defmethod monster-show ((m hydra))
  (print-on-sdl (format nil
                        "A malicious hydra with ~d heads."
                        (monster-health m))))

(defmethod monster-hit ((m hydra) x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (print-on-sdl "The corpse of the fully decapitated and decapacitated hydra falls to the floor!")
      (print-on-sdl (format nil "You lop off ~d of the hydra's heads!" x))))

(defmethod monster-attack ((m hydra))
  (let ((x (randval (ash (monster-health m) -1))))
    (print-on-sdl (format nil "A hydra attacks you with ~d of its heads! It also grows back one more head! " x))
    (incf (monster-health m))
    (decf *player-health* x)))

