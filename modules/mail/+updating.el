;; -*- lexical-binding: t -*-

;; -----------------------------------------------------------------------------
;;; Commentary
;;
;; Functions related to updating emacs mu4e and components:
;;   - on dbus signal   :: onMaildirChanged
;;        -> update mu index, update everything
;;   - send dbus signal :: muIndexUpdated
;;        -> (notifies MuDirWatcher to send notifications, update trayicon)
;;
;; -----------------------------------------------------------------------------

;;; dbus

;;;; client  : onMaildirChanged

(defun callback (s)
  (message s))

(defun +mail-connect-dbus-imapd ()
  
  )

;; (dbus-register-signal  :session
;; 					   "com.mail.imapd"
;; 					   "/"
;; 					   "interface"
;; 					   "MaildirChanged"
;; 					   'callback)

;; ...need to continously poll to see if com.mail.imapd is still up...
;; ..., restart it, then re-register that signal...


;;;; service : muIndexUpdated



(defun fn-mu4e-fix-maildir-names ()
  "Remove INBOX. prefix from mu4e maildir names..."
  (loop for m in mu4e-maildirs-extension-maildirs
		do (let ((name (plist-get m :name)))
			 (when (s-prefix? "INBOX." name ))
			 (plist-put m :name ( s-chop-prefix "INBOX." name )))))

;; TODO:  fix-maildir-names on `mu4e-maildirs-extension-index-updated-handler',
;;        then remove from keymap lambda d

(defun fn-mu4e-update-sidebar ()
  "Update mu4e-maildirs, names, and refresh sidebar."
  (interactive)
  (setq mu4e-maildirs-extension-bookmarks nil)
  (setq mu4e-maildirs-extension-maildirs nil)
  (mu4e-maildirs-extension-update)
  (mu4e-maildirs-extension-unqueue-maybe)
  (fn-mu4e-fix-maildir-names)
  (mail-sidebar-buffer--refresh)
  (fn-mu4e-update-unread-counts)
  )

(defun fn-mu4e-on-updated-index ()
  ;; TODO: not sure if I need this...
  ;; (message "-- fn-mu4e-on-updated-index ------------!")
  ;; (fn-mu4e-update-sidebar)
  ;; (run-with-idle-timer 10 nil #'(lambda ()
  ;; 								  (start-process "offlineimap" nil "offlineimap")))

  ;; -> dbus notification!
  )

(add-hook 'mu4e-index-updated-hook #'fn-mu4e-on-updated-index)
(remove-hook 'mu4e-mark-execute-pre-hook #'fn-mu4e-on-updated-index)

(defvar fn--mu4e-offlineimap-call-scheduled nil)
(defun fn-mu4e-on-execute-hook (mark message)
  ;; (message "-- fn-mu4e-on-execute-hook -- !")
  (when (not fn--mu4e-offlineimap-call-scheduled)
	(setq fn--mu4e-offlineimap-call-scheduled t)
	(run-with-idle-timer 10 nil #'(lambda ()
									(setq fn--mu4e-offlineimap-call-scheduled nil)
									(mu4e-update-index)
									(start-process "offlineimap" nil "offlineimap")))
	)
  )

(add-hook 'mu4e-mark-execute-pre-hook #'fn-mu4e-on-execute-hook)

(defun fn-mu4e-update-unread-counts ()
  "Maildirs extension in this setup does not seem to properly
update unread counts for configured folders. This function will
do so."
  (loop for m in mu4e-maildirs-extension-maildirs
		do
		(plist-put m :unread
				   (let* ((path (plist-get m :path))
						  (cmd (mu4e-maildirs-extension-maildir-command path "AND flag:unread")))
					 (string-to-number (shell-command-to-string cmd))))))
