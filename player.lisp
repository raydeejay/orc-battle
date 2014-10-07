;;; player.lisp

(in-package #:orc-battle)

(defvar *player-health* nil)
(defvar *player-agility* nil)
(defvar *player-strength* nil)

;; player related functions
(defun init-player ()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead ()
  (<= *player-health* 0))

(defun show-player ()
  (print-on-sdl (format nil
                        "You are a valiant knight with a health of ~d, "
                        *player-health*)
                :x 2 :y 10 :color sdl:*yellow*)
  (print-on-sdl (format nil
                        "an agility of ~d, and a strength of ~d."
                        *player-agility*
                        *player-strength*)
                :x 2 :y 11 :color sdl:*yellow*)
  (sdl:update-display))

;; turn function
(defun old-player-attack ()
  (print-on-sdl "Attack style: [s]tab [d]ouble swing [r]oundhouse:"
                :x 1 :y 30 :color sdl:*cyan*)
  (let ((input (read-line)))
    (cond ((string-equal input "s")
           (stab))
          ((string-equal input "d")
           (double-swing))
          ((string-equal input "r")
           (roundhouse))
          ((string-equal input "q")
           (sdl:push-quit-event))
          (t (player-attack)))))

