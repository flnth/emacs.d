;;; tmux-notify.el                                  -*- lexical-binding: t -*-
;;
;; Filename: tmux-notify.el
;;
;; Copyright (C) 2018 Florian Thevißen
;;
;; Description: Tools for showing notifications and status in tmux
;; Author: Florian Thevißen <mail@florian-thevissen.de>
;; Keywords: lisp, tools
;; Created: Sun Oct 28 17:15:36 2018 (+0100)
;; Version: 0.1.0
;; Package-Required: TODO
;; URL: TODO
;;

;;;; notifications on message
(defun +tmux-notify-erc-on-message (nickname message buffer)
  "Called to display chat message as tmux notification."
  (let* ((nickname (s-trim nickname))
		 (length-nickname (length nickname))
		 (message (s-collapse-whitespace
				   (s-truncate (- +tmux-notify-notification-length length-nickname 2)
							   message)))
		 (message-length (min +tmux-notify-notification-length (+ length-nickname 2 (length message))))
		 (pad-length (max 0 (- +tmux-notify-notification-length message-length)))
		 (tmux-text (s-pad-right (+ +tmux-notify-notification-length 26)
								 " " (concat "#[fg=colour1]" nickname ": "
											 "#[fg=colour7]" message))))
	(+tmux-notify-show-notification tmux-text +tmux-notify-notification-default-time)
	;; (message tmux-text)
	;; (message "length: %s" (- (length tmux-text) 26))
	))

(add-hook 'ercn-notify-hook '+tmux-notify-erc-on-message)

;;;; server connection status

(defvar +tmux-notify-erc-monitored-servers-alist
  '(("kornbluth.freenode.net" . "*irc-freenode*")
	("liquid.oftc.net" . "*irc-OFTC*")
	("h2712310.stratoserver.net" . "h2712310.stratoserver.net:6667"))
  "List of monitored server names and buffer name pairs.")

(defvar +tmux-notify-erc-connected-servers '()
  "List of servers currently connected to.")

(defun +tmux-notify-erc-get-string ()
  "Checks all server buffers for connections, and constructs tmux
string."
  (string-join
   (loop for (server . buffer) in +tmux-notify-erc-monitored-servers-alist
		 nconc
		 (list (if (and (buffer-live-p (get-buffer buffer))
						(erc-server-process-alive buffer))
				   "#[fg=colour2]c"
				 "#[fg=colour1]n")))))

(defun +tmux-notify-erc-update-indicator ()
  (+tmux-notify-indicator-update 'tmux-notify-erc (+tmux-notify-erc-get-string)))

(defun +tmux-notify-erc-on-erc-connect (server nick)
  (+tmux-notify-erc-update-indicator))

(defun +tmux-notify-erc-on-erc-disconnect (nick ip reason)
  (+tmux-notify-erc-update-indicator))

(add-hook 'erc-after-connect '+tmux-notify-erc-on-erc-connect)
(add-hook 'erc-disconnected-hook '+tmux-notify-erc-on-erc-disconnect)

;;;;; initialize

(provide 'ui/tmux/tmux-notify-erc)
