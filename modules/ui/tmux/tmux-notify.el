;;; +tmux-notify.el                                  -*- lexical-binding: t -*-
;;
;; Filename: +tmux-notify.el
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
;; -----------------------------------------------------------------------------
;; <notification-area>     <indicators>      <date time>
;; <----l1-----------><s1-><---l2-----><-s2-><--const-->
(defvar +tmux-notify-notification-length 100)
(defvar +tmux-notify-spacer1-length 20)
(defvar +tmux-notify-indicators-length 3)
(defvar +tmux-notify-spacer2-length 1)
;; -----------------------------------------------------------------------------

(defvar +tmux-notify--current-notification-string "")
(defvar +tmux-notify--current-indicator-string "")
;; TODO: get date-time-string from tmux via `tmux show -g | grep status-right`
(defvar +tmux-notify-date-time-string "#[fg=colour250][#[fg=colour10]%d.%m.%Y#[fg=colour173] #[fg=colour38]%H:%M#[fg=colour250]]")

(defun +tmux-notify-update-status-right ()
  (let ((s   (concat "tmux_show_text '"
					 +tmux-notify--current-notification-string
					 (s-pad-left +tmux-notify-spacer1-length " " "")
					 +tmux-notify--current-indicator-string
					 (s-pad-left +tmux-notify-spacer2-length " " "")
					 +tmux-notify-date-time-string
					 "'")))
	(shell-command-to-string s)
	;; (message "%s" s)
	))

;;;; notifications

(defvar +tmux-notify-notification-default-time 60
  "The time a tmux notification is shown by-default.")

(setq +tmux-notify--reset-notification-timer nil)

(defun +tmux-notify-show-notification (msg time)
  "Shows message MSG in the top right of all tmux instances for
  TIME seconds, or indefinitely if TIME is nil."
  (setf +tmux-notify--current-notification-string msg)
  (+tmux-notify-update-status-right)
  (when +tmux-notify--reset-notification-timer		; remove existing timer
	(cancel-timer +tmux-notify--reset-notification-timer)
	(setf +tmux-notify--reset-notification-timer nil))
  (when time
	(setf +tmux-notify--reset-notification-timer		; add new timer to reset
		  (run-with-timer time nil '+tmux-notify-clear-notification))))

(defun +tmux-notify-clear-notification ()
  "Clears the tmux message, restoring the status-right-string."
  (setf +tmux-notify--current-notification-string "")
  (+tmux-notify-update-status-right))

;;;; indicators
(defstruct
	(+tmux-notify-indicator
	 (:type vector)
	 :named
	 (:constructor nil)
	 (:constructor +tmux-notify-indicator-create
				   (&key name (id (intern name)) callback (state "") position))
	 )
  "A status indicator shown in the tmux status bar."
  name									; string
  id									; symbol, constructed from name
  callback								; lambda to return current string
  state									; string
  position								; integer
  )

(setq +tmux-notify--indicators '())

(defun +tmux-notify--indicators-sort ()
  "Sort registered indicators according to their position
  property."
  (setf +tmux-notify--indicators
		(sort +tmux-notify--indicators
			  (lambda (a b) (< (+tmux-notify-indicator-position (cdr a))
						  (+tmux-notify-indicator-position (cdr b)))))))

(defun +tmux-notify-indicator-add (indicator &optional silent-on-duplicate)
  "Adds INDICATOR to the list of tracked indicators."
  (if (asoc-get +tmux-notify--indicators (+tmux-notify-indicator-id indicator))
	  (progn
		(when (not silent-on-duplicate)
  		  (error (concat "+tmux-notify-indicator-add:  an indicator called "
  						 (+tmux-notify-indicator-name indicator)
  						 " is already registered." ))))
	(progn
	  (push `(,(+tmux-notify-indicator-id indicator) . ,indicator)
			+tmux-notify--indicators)
	  (+tmux-notify--indicators-sort))))

(defun +tmux-notify-indicator-remove (indicator)
  "Removes indicator identified by ID from the list of tracked
  indicators. ID can be a string, symbol or +tmux-notify-indicator
  instance."
  (let* ((key (cond
			   ((stringp indicator) (intern indicator) )
			   ((symbolp indicator) indicator )
			   ((+tmux-notify-indicator-p indicator) (+tmux-notify-indicator-id indicator))))
		 (ind (asoc-get +tmux-notify--indicators key)))
	(if (null ind)
		(error (concat "tmux-notify-indicator-remove:  no indicator "
					   key
					   " registered. Cannot remove."))
	  (setf +tmux-notify--indicators (delq (assoc key +tmux-notify--indicators))))))

(defun +tmux-notify-indicators-poll ()
  "Polls all indicators and updates their state."
  (cl-loop for (key . indicator) in +tmux-notify--indicators collect
				  (let ((str (funcall (+tmux-notify-indicator-callback indicator))))
					(setf (+tmux-notify-indicator-state indicator) str))))

(defun +tmux-notify-indicators-update ()
  "Updates the tmux bar from the STATEs of registered indicators."
  (let ((notification-strings
		 (cl-loop for (key . indicator) in +tmux-notify--indicators collect
				  (+tmux-notify-indicator-state indicator))))
	(setf +tmux-notify--current-indicator-string
		  (string-join notification-strings " "))
	(+tmux-notify-update-status-right)))

(defun +tmux-notify-indicator-update (id str)
  "Update the indicator identified by symbol ID with string
  STR, then update tmux display."
  (when (not (asoc-get +tmux-notify--indicators id))
	(error (concat  "+tmux-update-indicator:  no indicator " (symbol-name id) " exists.")))
  (setf (+tmux-notify-indicator-state (asoc-get +tmux-notify--indicators id)) str)
  (+tmux-notify-indicators-update))

;; clear everything on exit

(defun +tmux-notify-clear ()
  (interactive)
  (setf +tmux-notify--current-notification-string "")
  (setf +tmux-notify--current-indicator-string "")
  (+tmux-notify-update-status-right))

(add-hook 'kill-emacs-hook #'+tmux-notify-clear)

;;  
;;  
;;  

;; TODO: make module such that it accepts functions it calls


(provide 'ui/tmux/tmux-notify)
