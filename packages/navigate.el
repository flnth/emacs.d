;;; navigate.el --- Seamlessly navigate between Emacs and tmux

;; Author:   Keith Smiley <keithbsmiley@gmail.com>
;; Created:  April 25 2014
;; Version:  0.1.5
;; Keywords: tmux, evil, vi, vim

;;; Commentary:

;; This package is inspired by vim-tmux-navigator.
;; It allows you to navigate splits in evil mode
;; Along with tmux splits with the same commands
;; Include with:
;;
;;    (require 'navigate)
;;

;;; Code:

(require 'evil)

(defgroup navigate nil
  "seamlessly navigate between Emacs and tmux"
  :prefix "navigate-"
  :group 'evil)

; Without unsetting C-h this is useless
;; (global-unset-key (kbd "C-h"))

; This requires windmove commands
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(defun tmux-navigate (direction)
  (let
    ((cmd (concat "windmove-" direction)))
      (condition-case nil
          (funcall (read cmd))
        (error
          (tmux-command direction)))))

(defun tmux-command (direction)
  (shell-command-to-string
    (concat "tmux select-pane -"
      (tmux-direction direction))))

(defun tmux-direction (direction)
  (upcase
    (substring direction 0 1)))


(provide 'navigate)

;;; navigate.el ends here
