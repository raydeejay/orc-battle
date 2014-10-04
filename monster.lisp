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
  (princa :cyan :bold "Monster #: " :reset)
  (finish-output nil)
  (let ((x (parse-integer (read-line))))
    (fresh-line)
    (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
        (progn (princa "That is not a valid monster number.")
               (fresh-line)
               (pick-monster))
        (let ((m (aref *monsters* (1- x))))
          (if (monster-dead m)
              (progn (princa "That monster is already dead.")
                     (fresh-line)
                     (pick-monster))
              m)))))

(defun show-monsters ()
  (print-on-sdl "Your foes:" :x 1 :y 13 :color sdl:*red*)
  (let ((n 1))
    (map 'list
         (lambda (m)
           (print-on-sdl (format nil "~2d. " n)
                         :x 5 :y (+ 14 n))
           (if (monster-dead m)
               (print-on-sdl "**dead**" :x 9 :y (+ 14 n) :color sdl:*blue*)
               (progn (print-on-sdl (format nil "[~2d]" (monster-health m) 3))
                      ;;(monster-show m)
                      ))
           (incf n))
         *monsters*)))

(defun old-show-monsters ()
  (fresh-line)
  (princa :red :bold "Your foes:" :reset)
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

