;;;; monster.lisp

(in-package #:orc-battle)

(defvar *monsters* nil)
(defvar *monster-builders* nil)
(defvar *monster-num* 12)

;; monster class
(defstruct monster (health (randval 10)))

(defgeneric monster-dead (m))

(defmethod monster-dead (m)
  (<= (monster-health m) 0))

(defgeneric monster-hit (m x))

(defmethod monster-hit (m x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (progn (princa :green
                     "You killed the "
                     (type-of m)
                     "! "
                     :reset))
      (progn (princa "You hit the "
                     (type-of m)
                     ", knocking off "
                     x
                     " health points! ")))
  (fresh-line))

(defgeneric monster-show (m))

(defmethod monster-show (m)
  (princa "A fierce " (type-of m)))

(defgeneric monster-attack (m))

(defmethod monster-attack (m))

;; functions that deal with monsters
(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
        (random-monster)
        m)))

(defun pick-monster ()
  (fresh-line)
  (princa :cyan "Monster #: " :reset)
  (finish-output nil)
  (let ((x (read)))
    (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
        (progn (princa "That is not a valid monster number.")
               (pick-monster))
        (let ((m (aref *monsters* (1- x))))
          (if (monster-dead m)
              (progn (princa "That monster is already dead.")
                     (pick-monster))
              m)))))

(defun show-monsters ()
  (fresh-line)
  (princa :red "Your foes:" :reset)
  (let ((x 0))
    (map 'list
         (lambda (m)
           (fresh-line)
           (princa "    " :bold (format nil "~2d" (incf x)) ". " :reset)
           (if (monster-dead m)
               (princa :black :bold "**dead**" :reset)
               (progn (princa "[" (format nil "~2d" (monster-health m)) "] ")
                      (monster-show m))))
         *monsters*)))

(defun monsters-dead ()
  (every #'monster-dead *monsters*))

;; monster initialization
(defun init-monsters ()
  (setf *monsters*
        (map 'vector
             (lambda (x)
               (declare (ignore x))
               (funcall (nth (random (length *monster-builders*))
                             *monster-builders*)))
             (make-array *monster-num*))))

