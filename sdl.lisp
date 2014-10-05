;;;; orc-battle.lisp

(in-package #:orc-battle)

(defvar *current-x* 0)
(defvar *current-y* 0)

;; test sdl stuff
(defun print-on-sdl (str &key (x *current-x*) (y *current-y*) (color sdl:*white*))
  (sdl:draw-string-solid-* str
                           (* x (sdl:char-width sdl:*default-font*))
                           (* y (sdl:char-height sdl:*default-font*))
                           :color color)
;;  (sdl:update-display)
  (setf *current-x* (+ x (length str)))
  (setf *current-y* y)
  (when (>= *current-x* *console-width*)
    (setf *current-x* 0)
    (incf *current-y*)))

(defun x-in-pixels (x)
  (* x (sdl:char-width sdl:*default-font*)))

(defun y-in-pixels (y)
  (* y (sdl:char-height sdl:*default-font*)))

(defun clear ()
  (sdl:clear-display sdl:*black*))

(defun clear-rectangle (x y w h)
  (sdl:draw-box-* (x-in-pixels x) (y-in-pixels y)
                  (x-in-pixels w) (y-in-pixels h)
                  :color sdl:*black*))
