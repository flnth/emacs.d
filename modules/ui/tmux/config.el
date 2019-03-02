;; modules/ui/tmux/config.el   -*- lexical-binding: t; -*-

;;; tmux
(use-package ui/tmux/tmux
  :after (s dash)
  :demand t
  ;; :bind (
  ;; 		 )
  ;; :config

  )


(defun tmux-create-pane (direction)
  (shell-command-to-string
   (concat "tmux split-window "
		   (cond ((string= direction "left")  "-bh ")
				 ((string= direction "right") "-h ")
				 ((string= direction "down")  "-v ")
				 ((string= direction "up")    "-bv ")
				 )
		   " -t "
		   (+tmux-combined-session-name-window-id)
		   " -c \'"
		   default-directory
		   "\'"
		   )))
(defun export-tmux-socket-path-cmd ()
  (when (not (boundp' __export-tmux-socket-path-cmd))
	(setq __export-tmux-socket-path-cmd
		  (concat "export TMUX="
				  (shell-command-to-string "tmux display-message -p '#{socket_path}'")
				  ";")))
  __export-tmux-socket-path-cmd)
;; TODO: use emamux instead of my own stuff to fix the window/pane problems I'm having?
(load-file (concat dir_system "/emacs/emamux.el"))

;;; tmux-notify

(use-package ui/tmux/tmux-notify
  :after (ui/tmux/tmux)
  )

(use-package ui/tmux/tmux-notify-erc
  :after (ui/tmux/tmux-notify)

  :config
  (+tmux-notify-indicator-add
   (+tmux-notify-indicator-create :name "tmux-notify-erc" ;TODO: remove hardcoded
								  :callback (lambda () (+tmux-notify-erc-get-string))
								  :position 1) t)
  (+tmux-notify-indicators-poll)
  (+tmux-notify-indicators-update))
