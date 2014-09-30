;;;; slime-mold.lisp

(in-package #:orc-battle)

;; slime mold
(defstruct (slime-mold (:include monster)) (sliminess (randval 5)))
(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((m slime-mold))
  (princa "A slime mold with a sliminess of ")
  (princa (slime-mold-sliminess m)))

(defmethod monster-attack ((m slime-mold))
  (let ((x (randval (slime-mold-sliminess m))))
    (princa "A slime mold wraps around your legs and decreases your agility by ")
    (princa x)
    (princa "! ")
    (fresh-line)
    (decf *player-agility* x)
    (when (zerop (random 2))
      (princa "It also squirts in your face, taking away a health point! ")
      (fresh-line)
      (decf *player-health*))))

