;;;; orc-battle.lisp

(in-package #:orc-battle)

;;; "orc-battle" goes here. Hacks and glory await!

(defparameter *console-width* 80)
(defparameter *console-height* 40)

(defparameter *game-mode* nil)
(defparameter *input-modes* ())
(defparameter *text-input* "")

;; a little helper function
(defun randval (n)
  (1+ (random (max 1 n))))

(defun init-game ()
  (setf *random-state* (make-random-state t))
  (setf *game-mode* #'player-attack)
  (setf *input-modes* ())
  (setf *text-input* "")
  (push #'global-input-mode *input-modes*)
  (push #'attack-input-mode *input-modes*))

;; exported function to start the game
(defun start ()
  (init-game)
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
  (declare (ignorable unicode))
  (clear-rectangle 1 40 23 3)
  (when (sdl:key= key :sdl-key-s)
    (print-on-sdl "Pressed key S!" :x 1 :y 40))
  (when (sdl:key= key :sdl-key-d)
    (print-on-sdl "Pressed key D!" :x 1 :y 40))
  (when (sdl:key= key :sdl-key-r)
    (print-on-sdl "Pressed key R!" :x 1 :y 40))
  (when (member key '(:sdl-key-s :sdl-key-d :sdl-key-r))
    (pop *input-modes*)
    (push #'pickup-monster-input-mode *input-modes*))
  ;;(print-on-sdl (format nil "From unicode: ~c ~d!" (code-char unicode) unicode)
  ;;              :x 1 :y 41)
  ) ; <-- careful

(defun pickup-monster-input-mode (key unicode)
  (clear-rectangle 1 42 23 3)
  (when (and (sdl:key= key :sdl-key-backspace)
             (> (length *text-input*) 0))
    (setf *text-input* (subseq *text-input* 0 (1- (length *text-input*)))))
  (when (<= (char-code #\0) unicode (char-code #\9))
    (setf *text-input* (format nil "~a~c"
                               *text-input*
                               (code-char unicode))))
  (print-on-sdl (format nil "Monster#: ~a" *text-input*)
                :x 1 :y 42
                :color sdl:*cyan*))

(defun player-attack ()
  (print-on-sdl "Attack style: [s]tab [d]ouble swing [r]oundhouse:"
                :x 1 :y 30 :color sdl:*cyan*))

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
