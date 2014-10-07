;;;; orc-battle.lisp

(in-package #:orc-battle)

;;; "orc-battle" goes here. Hacks and glory await!

(defparameter *console-width* 80)
(defparameter *console-height* 40)

(defparameter *input-mode* nil)

(defparameter *input-modes* ())

;; a little helper function
(defun randval (n)
  (1+ (random (max 1 n))))

;; exported function to start the game
(defun start ()
  (setf *random-state* (make-random-state t))
  (setf *input-mode* 'attack)
  (push #'attack-input-mode *input-modes*)
  (push #'global-input-mode *input-modes*)
  (init-monsters)
  (init-player)
  (sdl:with-init ()
    (sdl:initialise-default-font sdl:*font-10x20*)
    (sdl:window 640 480 :title-caption "Orc Battle")
    (sdl:resize-window (x-in-pixels *console-width*)
                       (y-in-pixels *console-height*))
    (print-on-sdl "* ORC BATTLE *" :x 23 :y 2)
    (sdl:set-default-font (sdl:initialise-font sdl:*font-8x8*))
    (sdl:enable-unicode)   ; may be better to enable it only when reading strings
    (game-loop))
  (fresh-line)
  (end-game))

;; input modes
(defun global-input-mode (key unicode)
  (declare (ignore unicode))
  (when (or (sdl:key= key :sdl-key-escape)
            (sdl:key= key :sdl-key-q))
    (sdl:push-quit-event)))

(defun attack-input-mode (key unicode)
  (clear-rectangle 1 40 23 2)
  (when (sdl:key= key :sdl-key-s)
    (print-on-sdl "Pressed key S!" :x 1 :y 40))
  (when (sdl:key= key :sdl-key-d)
    (print-on-sdl "Pressed key D!" :x 1 :y 40))
  (when (sdl:key= key :sdl-key-r)
    (print-on-sdl "Pressed key R!" :x 1 :y 40))
  (print-on-sdl (format nil "From unicode: ~c ~d!" (code-char unicode) unicode)
                :x 1 :y 41))

;; game loop
(defun game-loop ()
  (sdl:with-events (:poll)
    (:video-expose-event () (sdl:update-display))
    (:quit-event () t)
    (:key-down-event (:key key :unicode unicode)
                     (mapc (lambda (x)
                             (funcall x key unicode))
                           *input-modes*)
                     (princ unicode))
    (:idle ()
           (show-player)
           (show-monsters)
           (when (eq *input-mode* 'attack)
             (print-on-sdl "Attack style: [s]tab [d]ouble swing [r]oundhouse:" :x 1 :y 30)
             (sdl:update-display)))))

(defun end-game ()
  (when (player-dead)
    (princa :red "You have been killed. Game Over." :reset))
  (when (monsters-dead)
    (princa :green :bold "Congratulations! You have vanquished all of your foes." :reset)
    (fresh-line)))

;; main loop
(defun old-game-loop ()
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
    (game-loop))

  (when (or (player-dead) (monsters-dead))
    (sdl:push-quit-event)))

(defun main (argv)
  (declare (ignore argv))
  (start))
