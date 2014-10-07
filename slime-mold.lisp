;;;; slime-mold.lisp

(in-package #:orc-battle)

;; slime mold
(defstruct (slime-mold (:include monster)) (sliminess (randval 5)))
(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((m slime-mold))
  (print-on-sdl (format nil "A slime mold with a sliminess of ~d" (slime-mold-sliminess m))))

(defmethod monster-attack ((m slime-mold))
  (let ((x (randval (slime-mold-sliminess m))))
    (print-on-sdl (format nil "A slime mold wraps around your legs and decreases your agility by ~d!" x))
    (decf *player-agility* x)
    (when (zerop (random 2))
      (print-on-sdl "It also squirts in your face, taking away a health point! ")
      (decf *player-health*))))

