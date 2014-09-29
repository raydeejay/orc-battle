;;;; orc-battle.lisp

(in-package #:orc-battle)

;;; "orc-battle" goes here. Hacks and glory await!

;; selectively run code if inside/outside SLIME
;; #+:swank ()
;; #-:swank ()

;; a little helper function
(defun randval (n)
  (1+ (random (max 1 n))))

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

;; brigand

(defstruct (brigand (:include monster)))
(push #'make-brigand *monster-builders*)

(defmethod monster-attack ((m brigand))
  (let ((x (max *player-health* *player-agility* *player-strength*)))
    (cond ((= x *player-health*)
           (princa "A brigand hits you with his slingshot, taking off 2 health points! ")
           (fresh-line)
           (decf *player-health* 2))
          ((= x *player-agility*)
           (princa "A brigand catches your leg with his whip, taking off 2 agility points! ")
           (fresh-line)
           (decf *player-agility* 2))
          ((= x *player-strength*)
           (princa "A brigand cuts your arm with his whip, taking off 2 strength points!")
           (fresh-line)
           (decf *player-strength* 2)))))

;; turn function
(defun player-attack ()
  (fresh-line)
  (princa :cyan :bold "Attack style: [s]tab [d]ouble swing [r]oundhouse: " :reset)
  (finish-output nil)
  (case (read-char)
    (#\s (monster-hit (pick-monster)
                      (+ 2 (randval (ash *player-strength* -1)))))
    (#\d (let ((x (randval (truncate (/ *player-strength* 6)))))
           (princa "Your double swing has a strength of " x)
           (fresh-line)
           (monster-hit (pick-monster) x)
           (unless (monsters-dead)
             (monster-hit (pick-monster) x))))
    (#\r (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
           (unless (monsters-dead)
             (monster-hit (random-monster) 1))))
    (otherwise (player-attack))))

;; monster initialization
(defun init-monsters ()
  (setf *monsters*
        (map 'vector
             (lambda (x)
               (declare (ignore x))
               (funcall (nth (random (length *monster-builders*))
                             *monster-builders*)))
             (make-array *monster-num*))))

;; main loop
(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
        (show-monsters)
        (player-attack)))
    (fresh-line)
    (map 'list
         (lambda (m)
           (or (monster-dead m) (monster-attack m)))
         *monsters*)
    (game-loop)))

;; exported function to start the game
(defun start ()
  (init-monsters)
  (init-player)
  (game-loop)
  (fresh-line)
  (when (player-dead)
    (princa :red "You have been killed. Game Over." :reset))
  (when (monsters-dead)
    (princa :green :bold "Congratulations! You have vanquished all of your foes." :reset))
  (fresh-line)
  (sb-ext:quit))

