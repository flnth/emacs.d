;; -*- lexical-binding: t -*-

(setq emacs-executable (concat (getenv "DIR_LOCAL") "/bin/emacsclient"))
(setq emacs-socket "/tmp/emacs1000/server") ;; TODO: make part of environment

(setq imapd-callbacks `(,(concat "imapfilter -c " (getenv "DIR_SYSTEM") "/mail/imapfilter.lua")
						"offlineimap"
						,(concat emacs-executable " --socket-name=" emacs-socket " -e (imapd-callback)")))

(defun imapd-callback ()
  "Function called by imapd."
  (mu4e-update-index)
  (run-with-timer 2 nil #'(lambda ()
							(fn-mu4e-update-sidebar))))

(defun imapd (&optional action)
  (let ((found nil))
	(loop for p in (process-list)
		  when (string= (process-name p) "imapd")
		  do (setq found t))
	(cond
	 ((eq action 'start)
	  (if found
		  (message "imapd already running")
		(apply 'start-process `("imapd" "python3" ,(concat (getenv "DIR_SYSTEM") "/imapd")
								,@imapd-callbacks))
		;; TODO: process sentinel to get notified about start/failure/exit
		))
	 ((eq action 'stop)
	  (if found
		  (progn
			(delete-process "imapd")
			(message "imapd stopped"))
		(message "imapd not running")))
	 ((eq action 'restart)
	  (if found
		  (progn
			(delete-process "imapd")
			(imapd 'start)
			(message "imapd restarted"))
		(message "imapd not running"))))))

(defun imapd-start ()
  "Starts an imapd process, if not already running."
  (interactive)
  (imapd 'start))

(defun imapd-restart ()
  "Restarts a running imapd process."
  (interactive)
  (imapd 'restart))

(defun imapd-stop ()
  "Stops an imapd process."
  (interactive)
  (imapd 'stop))

(defun imapd-running? ()
  (interactive)
  (let ((found nil))
	(loop for p in (process-list)
		  when (string= (process-name p) "imapd")
		  do (setq found t))
	found))

;; start imapd
;; (run-with-timer 2 nil #'(lambda ()
;; 						  (imapd-start)))
