;;; dbus-monitor.el                                  -*- lexical-binding: t -*-
;;
;; Filename: dbus-monitor.el
;;
;; Copyright (C) 2018 Florian Thevißen
;;
;; Description: A simple dbus monitoring tool
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
;; Tools to monitor dbus-services, and optionally starting and stopping
;; associated processes.
;;
;; -----------------------------------------------------------------------------

;; dbus-service
;;
;;
;;
;;;; dbus-monitor-service struct
(defstruct
	(+dbus-monitor-service
	 (:type vector)
	 :named
	 (:constructor nil)
	 (:constructor +dbus-monitor-service-create
				   (&key name
						 (path nil)
						 (interface nil)
						 (executable nil)
						 (policy (intern "monitor"))
						 (supervise nil)
						 (process nil)
						 (tracked-properties '() ))))
  "A monitored dbus-service."
  name				   ; string (i.e. net.pydbus.ClientServerExample)
  path				   ; string (i.e. /Node)
  interface			   ; string (i.e. sample.interface
  executable		   ; string || string list for arguments || lambda || symbol
  policy			   ; {monitor,keep-alive}
  supervise			   ; bool
  process			   ; process if started from within emacs, or nil
  tracked-properties					; string list
  (last-seen-running nil)				; bool
  )

(setf +dbus-monitor--services '())

(defun +dbus-monitor-add-service (s)
  (push s +dbus-monitor--services))

;;;; monitoring, actions

(defun +dbus-monitor-ping (s)
  (dbus-ping :session (+dbus-monitor-service-name s) 1)
  )

;;;; public interface

;;;;; monitoring app

(setq dbus-monitor-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "gr") #'+dbus-monitor-show)
	(evil-define-key '(normal insert visual) map
	  (kbd "gr") #'+dbus-monitor-show
	  (kbd "+") #'+dbus-monitor-show-add
	  (kbd "d") #'+dbus-monitor-show-remove)
	map))

(define-minor-mode dbus-monitor-mode
  "Minor mode for +dbus-monitor-show."
  :lighter " dbus-monitor"
  :keymap dbus-monitor-mode-map
  )

;; TODO: implement
(defun +dbus-monitor-show-add-new ()
  "Prompt for a name to add."
  (interactive)
  (error "Not implemented yet.")
  )

;; TODO: implement
(defun +dbus-monitor-show-remove ()
  "Remove the service on the current line."
  ;; get name of service on current line
  ;; remove that
  ;; refresh
  (error "Not implemented yet.")
  )

(defun +dbus-monitor-show ()
  "Show a summary of tracked services."
  (interactive)
  (pop-to-buffer-same-window (get-buffer-create "*dbus-monitor*"))
  (let ((inhibit-read-only nil)
		(cursor-pos (point)))
	(erase-buffer)
	(dbus-monitor-mode 1)
	(insert (format "%-42s %s\n" "service" "running"))
	(insert "--------------------------------------------------\n")
	(cl-loop for service in +dbus-monitor--services
			 do
			 (let ((running (+dbus-monitor-ping service)))
			   (insert (format "%-45s %s"
							   (+dbus-monitor-service-name service)
							   (if running "✓" "✗")
							   ))))
	(goto-char cursor-pos)))

(defun +dbus-monitor-all-running-p ()
  ""
  (let ((bs (cl-loop for service in +dbus-monitor--services
					 collect (+dbus-monitor-ping service))))
	(eval `(and ,@bs))))


;;;;; supervisor

(defvar +dbus-monitor-supervisor-check-interval 30
  "Time in seconds to check running dbus services.")

(setq +dbus-monitor--all-supervised-running-last nil)

(defsubst +dbus-monitor-xor (a b) (if a (if (not b) a) b))

(defun +dbus-monitor-supervisor-service-stopped-handler (service)
  ;; do something, e.g. monitor. Or call external hooks, or something.
  )

(defun +dbus-monitor-service-start (service)
  "Start SERVICE using its property EXECUTABLE which can be a
string to the executable, a string list containing a string to
the executable and arguments, a lambda or a symbol."
  (let ((ex (+dbus-monitor-service-executable service)))
	(cond
	 ((stringp ex) (progn (message "1")
						   (setf (+dbus-monitor-service-process service)
								 (start-process (concat "dbus-" (+dbus-monitor-service-name service))
												"*dbus-monitor-services*" ex))
						   nil
						   ))
	 ;; TODO: string list for arguments
	 ((or (functionp ex)
	 	  (symbolp ex)) (message "2") (funcall ex))
	 )))

(defun +dbus-monitor-service-stop (service)
  "Stop SERVICE from running, if running."
  (let* ((proc (+dbus-monitor-service-process service))
		 (procname (if proc (process-name proc) nil))
		 (status (if proc (process-status proc) nil)))
	(message (format "stopping dbus-service process %s with status %s..." procname status))
	(when (and proc
			   (equal (process-status proc) 'run))
	  (kill-process proc)))
  )

(defun +dbus-monitor-supervisor-start-all ()
  "Start all monitored dbus services that are not already
  running."
  (cl-loop for service in +dbus-monitor--services
		   do
		   (when (not (+dbus-monitor-ping service))
			 (+dbus-monitor-services-start service))))

(defun +dbus-monitor-supervisor-run ()
  "Check all services, act accordingly."
  (let ((all-supervised-running t))
	(cl-loop for service in +dbus-monitor--services
			 do
			 (when (+dbus-monitor-service-supervise service)
			   (let ((runstate `(,(+dbus-monitor-service-last-seen-running service)
								 ,(+dbus-monitor-ping service)))
					 (policy (+dbus-monitor-service-policy service)))
				 (pcase runstate
				   ;; `(last-seen-running running-now)
				   (`(t t) (progn
							 ;; still online
							 nil
							 )
					)
				   (`(t nil) (progn
							   ;; died
							   (+dbus-monitor-supervisor-service-stopped-handler)
							   (setf (+dbus-monitor-service-last-seen-running service) nil
									 all-supervised-running nil)
							   (when (eq policy 'keep-alive)
								 (+dbus-monitor-service-start service)
								 ;; schedule re-check asap
								 (run-with-timer 5 nil #'+dbus-monitor-supervisor-run))
							   ))
				   (`(nil t) (progn
							   ;; came back online
							   ))
				   (`(nil nil) (progn
								 ;; remains dead
								 (setf all-supervised-running nil)
								 ;; (when (eq policy 'keep-alive)
								 ;;   (+dbus-monitor-service-start service))
								 ))))))

	(when (+dbus-monitor-xor all-supervised-running
							 +dbus-monitor--all-supervised-running-last)
	  (message (format "dbus-monitor - all supervised running changed: %s") all-supervised-running)
	  ;; -> status leds green (e.g. tmux, traybar?)
	  (setf +dbus-monitor--all-supervised-running-last all-supervised-running))))

;;  ☯  ⚤   ⚪⚫ ⛉ ⛔ ⛝ ⚉ ⚇  ☑ ☒

(provide 'app/dbus-monitor/dbus-monitor)
