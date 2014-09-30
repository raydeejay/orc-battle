;;;; orc-battle.lisp

(in-package #:orc-battle)

;;; "orc-battle" goes here. Hacks and glory await!

;; selectively run code if inside/outside SLIME
;; #+:swank ()
;; #-:swank ()

;; a little helper function
(defun randval (n)
  (1+ (random (max 1 n))))

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
    (#\q (sb-ext:exit))
    (otherwise (read-char)
               (player-attack))))

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
  (setf *random-state* (make-random-state t))
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

