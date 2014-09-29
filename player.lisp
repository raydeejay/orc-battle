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
  (fresh-line)
  (princa :yellow
          "You are a valiant knight with a health of "
          *player-health*
          ", an agility of "
          *player-agility*
          ", and a strength of "
          *player-strength*
          "."
          :reset))

