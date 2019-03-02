;;; tmux.el                                            -*- lexical-binding: t -*-
;;
;; Filename: tmux.el
;;
;; Copyright (C) 2018 Florian Thevißen
;;
;; Description: tmux-utilities
;; Author: Florian Thevißen <mail@florian-thevissen.de>
;; Keywords: lisp, tools
;; Created: Sun Oct 28 17:15:36 2018 (+0100)
;; Version: 0.1.0
;; Package-Required: TODO
;; URL: TODO
;;
;; -----------------------------------------------------------------------------
;;; Commentary
;;
;; TODO
;;
;; -----------------------------------------------------------------------------
(message "loading /ui/tmux/tmux ...")

(defun +tmuxp ()
  "Check if current local environment of this emacs(client) is
  inside tmux."
  (interactive)
  (let* ((env (frame-parameter nil 'environment))
         (has-tmux-var-list (cl-mapcar (lambda (entry)
                                         (cl-search "TMUX" entry)) env)))
    (not (null (-reduce-from '(lambda (e1 e2) (or e1 e2))
                         nil
                         has-tmux-var-list)))))

(defun +tmux-client-width ()
  "Get the width of the current tmux client."
  (string-to-number (shell-command-to-string "tmux display-message -p '#{client_width}'")))

(defun +tmux-session-name ()
  "Get the name/id of the current tmux session."
  (s-chomp (shell-command-to-string "tmux display-message -p '#S'")))

(defun +tmux-combined-session-name-window-id ()
  "Get the name/id of current tmux session, and the window id of
  the current window."
  (s-chomp (shell-command-to-string "tmux display-message -p '#S:#I'")))


(message "... done!")
(provide 'ui/tmux/tmux)
