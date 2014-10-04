;;;; orc-battle.lisp

(in-package #:orc-battle)

;;; "orc-battle" goes here. Hacks and glory await!

;; a little helper function
(defun randval (n)
  (1+ (random (max 1 n))))

;; exported function to start the game
(defun start ()
  (setf *random-state* (make-random-state t))
  (init-monsters)
  (init-player)
  (sdl:with-init ()
    (sdl:window 640 480 :title-caption "Orc Battle")
    (sdl:initialise-default-font sdl:*font-8x8*)
    (sdl:draw-string-solid-* "hello hello" 10 10 :color sdl:*white*)
    (sdl:update-display)
    (sdl:with-events (:poll)
      (:quit-event () t)
      (:key-down-event (:key key)
                       (when (or (sdl:key= key :sdl-key-escape)
                                 (sdl:key= key :sdl-key-q))
                         (sdl:push-quit-event))
                       (when (sdl:key= key :sdl-key-a)
                         (print-on-sdl "Pressed key A!"))

                       )
      (:video-expose-event () (sdl:update-display))

      (game-loop)))
  (fresh-line)
  (end-game))

(defun end-game ()
  (when (player-dead)
    (princa :red "You have been killed. Game Over." :reset))
  (when (monsters-dead)
    (princa :green :bold "Congratulations! You have vanquished all of your foes." :reset)
    (fresh-line)))

;; test sdl stuff
(defun print-on-sdl (str)
  (sdl:draw-string-solid-* str 10 440 :color sdl:*white*)
  (sdl:update-display))

;; turn function
(defun player-attack ()
  (fresh-line)
  (princa :cyan :bold "Attack style: [s]tab [d]ouble swing [r]oundhouse: " :reset)
  (finish-output nil)
  (print-on-sdl "You are attacking now!")
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
    (game-loop))

  (when (or (player-dead) (monsters-dead))
    (sdl:push-quit-event)))


(defun main (argv)
  (declare (ignore argv))
  (start))
