;;;; orc-battle.lisp

(in-package #:orc-battle)

;; test sdl stuff
(defun print-on-sdl (str x y &optional (color sdl:*white*))
  (sdl:draw-string-solid-* str
                           (* x (sdl:char-width sdl:*default-font*))
                           (* y (sdl:char-height sdl:*default-font*))
                           :color color)
  (sdl:update-display))

(defun x-in-pixels (x)
  (* x (sdl:char-width sdl:*default-font*)))

(defun y-in-pixels (y)
  (* y (sdl:char-height sdl:*default-font*)))

