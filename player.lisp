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
(defun player-attack ()
  (fresh-line)
  (princa :cyan :bold "Attack style: [s]tab [d]ouble swing [r]oundhouse: " :reset)
  (finish-output nil)
  (print-on-sdl "You are attacking now!" 10 10)
  (let ((input (read-line)))
    (cond ((string-equal input "s")
           (monster-hit (pick-monster)
                        (+ 2 (randval (ash *player-strength* -1)))))
          ((string-equal input "d")
           (let ((x (randval (truncate (/ *player-strength* 6)))))
             (princa "Your double swing has a strength of " x)
             (fresh-line)
             (monster-hit (pick-monster) x)
             (unless (monsters-dead)
               (monster-hit (pick-monster) x))))
          ((string-equal input "r")
           (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
             (unless (monsters-dead)
               (monster-hit (random-monster) 1))))
          ((string-equal input "q")
           (sdl:push-quit-event))
          (t (player-attack)))))

