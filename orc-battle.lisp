;;;; orc-battle.lisp

(in-package #:orc-battle)

;;; "orc-battle" goes here. Hacks and glory await!

(defparameter *console-width* 80)
(defparameter *console-height* 40)

(defparameter *game-mode* nil)
(defparameter *input-modes* ())
(defparameter *text-input* "")
(defparameter *attack-type* nil)

(defparameter *font-big* (sdl:initialise-font sdl:*font-10x20*))
(defparameter *font-small* (sdl:initialise-font sdl:*font-8x8*))

;; a little helper function
(defun randval (n)
  (1+ (random (max 1 n))))

(defun init-game ()
  (setf *random-state* (make-random-state t))
  (setf *game-mode* #'player-attack)
  (setf *input-modes* ())
  (setf *text-input* "")
  (setf *attack-type* nil)
  (push #'global-input-mode *input-modes*)
  (push #'attack-input-mode *input-modes*))

;; exported function to start the game
(defun start ()
  (init-game)
  (init-monsters)
  (init-player)
  (sdl:with-init ()
    (sdl:window 640 480 :title-caption "Orc Battle")
    (sdl:set-default-font *font-small*)
    (sdl:resize-window (x-in-pixels *console-width*)
                       (y-in-pixels *console-height*))
    (sdl:set-default-font *font-big*)
    (print-on-sdl "* ORC BATTLE *" :x 23 :y 2)
    (sdl:set-default-font *font-small*)
    (sdl:enable-unicode)   ; may be better to enable it only when reading strings?
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
  (declare (ignorable unicode))
  (clear-rectangle 1 31 80 3)
  (when (sdl:key= key :sdl-key-s)
    (print-on-sdl "You choose to stab a monster!" :x 1 :y 31)
    (setf *attack-type* #'stab))
  (when (sdl:key= key :sdl-key-d)
    (print-on-sdl "You decide to attempt a double swing." :x 1 :y 31)
    (setf *attack-type* #'double-swing))
  (when (sdl:key= key :sdl-key-r)
    (print-on-sdl "Roundhouse of hits!" :x 1 :y 31)
    (setf *attack-type* #'roundhouse))
  (when (member key '(:sdl-key-s :sdl-key-d :sdl-key-r))
    (pop *input-modes*)
    (push #'pickup-monster-input-mode *input-modes*)
    (setf *game-mode* #'pick-monster)))

(defun pickup-monster-input-mode (key unicode)
  (clear-rectangle 1 32 80 3)
  (when (> (length *text-input*) 0)
    (when (sdl:key= key :sdl-key-backspace)
      (setf *text-input* (subseq *text-input* 0 (1- (length *text-input*)))))
    (when (or (sdl:key= key :sdl-key-return)
              (sdl:key= key :sdl-key-kp-enter))
      (let ((x (parse-integer *text-input*)))
        (when (< 0 x (1+ (length *monsters*)))
          (funcall *attack-type* (parse-integer *text-input* :junk-allowed t))
          (setf *text-input* "")
          (pop *input-modes*)
          (push #'attack-input-mode *input-modes*)
          (setf *game-mode* #'player-attack)))))
  (when (<= (char-code #\0) unicode (char-code #\9))
    (setf *text-input* (format nil "~a~c"
                               *text-input*
                               (code-char unicode))))
  (print-on-sdl (format nil "Monster #: ~a" *text-input*)
                :x 1 :y 32
                :color sdl:*cyan*))

(defun get-monster-by-number (n)
  (aref *monsters* (1- n)))

;; attack functions
(defun stab (n)
  (monster-hit (get-monster-by-number n)
               (+ 2 (randval (ash *player-strength* -1)))))

(defun double-swing (n)
  (let ((x (randval (truncate (/ *player-strength* 6)))))
    (print-on-sdl (format nil "Your double swing has a strength of ~d." x))
    (monster-hit (get-monster-by-number n) x)
    (unless (monsters-dead)
      (monster-hit (get-monster-by-number n) x))))

(defun roundhouse ()
  (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
    (unless (monsters-dead)
      (monster-hit (random-monster) 1))))

;; select an attack type
(defun player-attack ()
  (print-on-sdl "Attack style: [s]tab [d]ouble swing [r]oundhouse:"
                :x 1 :y 30 :color sdl:*cyan*))

;; select the monster to attack
(defun pick-monster ()
  (print-on-sdl "Monster #: " :x 1 :y 32 :color sdl:*cyan*))

;; select the monster to attack
(defun old-pick-monster ()
  (princa :cyan :bold "Monster #: " :reset)
  (finish-output nil)
  (let ((x (parse-integer (read-line))))
    (fresh-line)
    (if (not (and (integerp x) (>= x 1) (<= x *monster-num*)))
        (progn (print-on-sdl "That is not a valid monster number.")
               (pick-monster))
        (let ((m (aref *monsters* (1- x))))
          (if (monster-dead m)
              (progn (print-on-sdl "That monster is already dead.")
                     (pick-monster))
              m)))))

;; game loop
(defun game-loop ()
  (sdl:with-events (:poll)
    (:video-expose-event () (sdl:update-display))
    (:quit-event () t)
    (:key-down-event (:key key :unicode unicode)
                     (mapc (lambda (x)
                             (funcall x key unicode))
                           *input-modes*))
    (:idle ()
           (show-player)
           (show-monsters)
           (funcall *game-mode*)
           (sdl:update-display))))

(defun end-game ()
  (when (player-dead)
    (print-on-sdl "You have been killed. Game Over."))
  (when (monsters-dead)
    (print-on-sdl "Congratulations! You have vanquished all of your foes.")))

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
