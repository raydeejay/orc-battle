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
      (print-on-sdl(format nil "You killed the ~a!" (type-of m)))
      (print-on-sdl (format nil "You hit the ~a, knocking off ~d health points"
                            (type-of m) x))))

(defgeneric monster-show (m))

(defmethod monster-show (m)
  (print-on-sdl (format nil "A fierce ~a" (type-of m))))

(defgeneric monster-attack (m))

(defmethod monster-attack (m))

;; functions that deal with monsters
(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
        (random-monster)
        m)))

(defun show-monsters ()
  (print-on-sdl "Your foes:" :x 1 :y 13 :color sdl:*red*)
  (let ((n 1))
    (map 'list
         (lambda (m)
           (clear-rectangle 5 (+ 14 n) 48 1)
           (print-on-sdl (format nil "~2d. " n)
                         :x 5 :y (+ 14 n))
           (if (monster-dead m)
               (print-on-sdl "**dead**" :x 9 :y (+ 14 n) :color sdl:*magenta*)
               (progn (print-on-sdl (format nil "[~2d] " (monster-health m)))
                      (monster-show m)
                      ))
           (incf n))
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

