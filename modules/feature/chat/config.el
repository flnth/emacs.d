;; modules/feature/chat/config.el    -*- lexical-binding: t; -*-

(setq loc (getenv "LOC"))

;;; erc
(use-package erc
  :after (auth-source)
  :commands (+chat-irc-connect-bitlbee
			 +chat-new-frame)
  
  :bind (:map erc-mode-map
			  ("C-p" .  #'erc-previous-command)
			  ("C-n" .  #'erc-next-command))

  :init
  ;; NOTE: hack
  (defvar erc-server-responses (make-hash-table :test #'equal)
	"Hash table mapping server responses to their handler hooks.")

  :config
  (require 'erc-backend)
  (require 'erc-sasl)
  (require 'erc-goodies)

  (defun +chat-new-frame (name)
	(set-frame-name name)
	(let ((bitlbee-buf (get-buffer "&bitlbee")))
	  (when bitlbee-buf
		(switch-to-buffer bitlbee-buf))))

  (defun +chat-irc-connect-bitlbee ()
	(interactive)
	(let* ((auth (nth 0 (auth-source-search :host "h2712310.stratoserver.net"
											:port 6667
											:requires '(user secret))))
		   (pass (funcall (plist-get auth :secret))))
	  (when (null pass)
		(error "+chat-irc-connect-bitlbee:  could not get password from .authinfo.gpg"))
	  (erc :server "h2712310.stratoserver.net"
		   :port "6667"
		   :nick "fthevissen"
		   :password pass)))

  (add-to-list 'erc-sasl-server-regexp-list ".*")

;;;; appearances
  (setq erc-prompt (lambda () (concat "[" (buffer-name) "]")))

  (defun +chat--erc-on-mode-hook ()
	;; (buffer-face-set '(:background "#262626"))
	;; (hidden-mode-line-mode)
	(make-local-variable 'scroll-margin)
	(setq scroll-margin 1)
	(yascroll-bar-mode -1)
	)
  (add-hook 'erc-mode-hook '+chat--erc-on-mode-hook)

  (defun +chat--erc-line-highlight ()
	"Activate the Hl-Line overlay on the current line."
	(interactive)
	(progn
	  (setq hl-line-overlay (hl-line-make-overlay))
	  (overlay-put hl-line-overlay
				   'window (selected-window))
	  (hl-line-move hl-line-overlay)))

  (setq erc-mode-line-format "%t")
  (setq erc-fill-function 'erc-fill-static
		erc-fill-column 100
		erc-fill-static-center 14
		erc-fill-prefix 8
		erc-hl-nicks-color-contrast-strategy 'invert
		erc-hl-nicks-minimum-contrast-ratio 4.0
		)

  (setq erc-scrolltobottom-mode t)
  (setq erc-hide-list erc-track-exclude-types)

;;;; dynamic sizing of window width
  (defun +chat-irc-crop-column-fill ()
	(interactive)
	(save-excursion
	  (walk-windows (lambda (w)
					  (when (window-live-p w)
						(let ((buffer (window-buffer w)))
						  (set-buffer buffer)
						  (when (eq major-mode 'erc-mode)
							(let ((win-width (- (window-width w) 2))
								  (global-erc-fill-column (default-value 'erc-fill-column)))
							  (when (not (local-variable-p 'erc-fill-column))
								(make-variable-buffer-local 'erc-fill-column))
							  ;; (message "frame-width, win-width, erc-fill-column: %s, %s, %s" (frame-native-width) win-width erc-fill-column)
							  (cond ((> win-width global-erc-fill-column)
									 (setq erc-fill-column global-erc-fill-column)
									 ;; (message "adjusted to %s" erc-fill-column)
									 )
									(t
									 (setq erc-fill-column win-width)
									 ;; (message "adjusted to %s" erc-fill-column)
									 )
									))))))))
	(setq +chat--irc-crop-column-fill-scheduled nil))

  (defvar +chat--irc-crop-column-fill-scheduled nil)
  ;; TODO: ??
  (remove-hook 'window-configuration-change-hook (lambda () (when (not +chat--irc-crop-column-fill-scheduled)
														 (setq +chat--irc-crop-column-fill-scheduled t)
														 (run-with-idle-timer 0.5 nil #'+chat-irc-crop-column-fill))))

  (defun fn--irc-schedule-crop-column-fill-scheduled (&optional _)
	(when (not +chat--irc-crop-column-fill-scheduled)
	  (setq +chat--irc-crop-column-fill-scheduled t)
	  (run-with-idle-timer 0.5 nil #'+chat-irc-crop-column-fill)
	  ))

  (add-hook 'window-size-change-functions #'fn--irc-schedule-crop-column-fill-scheduled)

;;;; erc-track
  (require 'erc-track)

  ;; optional resetting of mode-line tracking
  ;; (defun reset-erc-track-mode ()
  ;; 	(interactive)
  ;; 	(setq erc-modified-channels-alist nil)
  ;; 	(erc-modified-channels-update))
  ;; (global-set-key (kbd "C-c r") 'reset-erc-track-mode)

  ;; promoting private query buffer tracking
  (defadvice erc-track-find-face (around erc-track-find-face-promote-query activate)
	(if (erc-query-buffer-p)
		(setq ad-return-value (intern "erc-current-nick-face"))
	  ad-do-it))

  (erc-track-mode 1)
  (setq erc-track-showcount nil)
  (setq erc-track-position-in-mode-line nil) ; t -> show in modeline

  ;; TODO: doesn't work, apparently. Do fix.
  ;;  update erc-modified-channels-alist on window-selection
  ;; (defun +chat--erc-on-buffer-list-update ()
  ;; 	(when (eq major-mode 'erc-mode)
  ;; 	  (message "---- +chat--erc-on-buffer-list-update")
  ;; 	  (erc-modified-channels-remove-buffer (current-buffer))
  ;; 	  (erc-modified-channels-display)))
  ;; (add-hook #'buffer-list-update-hook #'+chat--erc-on-buffer-list-update )

  ;; NOTE:  erc-track-list-changed-hook?
  ;; run whenever erc-modified-channels-alist changes

  ;; NOTE: erc-track-when-inactive?

  ;; NOTE: erc auto query:  look at how they do it to intercept (new) private messages?
  ;; Need to differentiate between
  ;;   - private messages
  ;;   - non-private messages

;;;; history access

  (setq erc-log-dir (concat dir_emacs
							"share/cache/erc-logs/"))

;;;; auto join

  ;; (defvar +chat-irc--startup-connect-delay 10)
  (defvar +chat-irc-bitlbee-autojoin-channels '("#developers" "#imittagessen"))

  (defun +chat-irc-bitlbee-join-channels ()
	(interactive)
	(message "-- irc:  joining bitlbee channels...")
	(let* ((bitlbee-buf (get-buffer "&bitlbee"))
		   (join-fun (lambda (chan)
					   (end-of-buffer)
					   (insert (concat "/join " chan))
					   (erc-send-current-line))))
	  (if (null bitlbee-buf)
		  (message "No &bitlbee buffer found!")
		(with-current-buffer bitlbee-buf
		  (cl-loop for chan in +chat-irc-bitlbee-autojoin-channels do
				   (if (null (get-buffer chan))
					   (progn
						 (message "   joining %s ..." chan )
						 (funcall join-fun chan)
						 (sit-for 0.5))
					 (message "   skipping %s ..." chan))))))
	(message "   ...done!" ))

  (when (string= (getenv "LOC") "work")
	(run-with-idle-timer +chat-irc--startup-connect-delay nil #'+chat-irc-bitlbee-join-channels) )

;;;; bitlbee
  (defun bitlbee-netrc-identify ()
	"Auto-identify for Bitlbee channels using authinfo or netrc.
(defvar +chat-irc--startup-connect-delay 30)
(run-with-idle-timer +chat-irc--startup-connect-delay nil #'+chat-irc-connect-bitlbee)
  The entries that we look for in netrc or authinfo files have
  their 'port' set to 'bitlbee', their 'login' or 'user' set to
  the current nickname and 'server' set to the current IRC
  server's name.  A sample value that works for authenticating
  as user 'keramida' on server 'localhost' is:

  machine localhost port bitlbee login keramida password supersecret"
	(interactive)
	(when (string= (buffer-name) "&bitlbee")
	  (let* ((secret (plist-get (nth 0 (auth-source-search :max 1
														   :host "bitlbee"
														   :user (erc-current-nick)
														   :port "bitlbee"))
								:secret))
			 (password (if (functionp secret)
						   (funcall secret)
						 secret)))
		(erc-message "PRIVMSG" (concat (erc-default-target) " " "identify" " " password) nil))))

  ;; overwrite ercn-match for it to also provide the erc buffer
  (defun fn-ercn-match ()
	"Extracts information from the buffer and fires `ercn-notify-hook' if needed."
	(save-excursion
	  (goto-char (point-min))
	  (let* ((vector (erc-get-parsed-vector (point-min)))
			 (nickuserhost (erc-get-parsed-vector-nick vector))
			 (nickname (and nickuserhost
							(nth 0 (erc-parse-user nickuserhost))))
			 (nick-beg (and nickname
							(re-search-forward (regexp-quote nickname)
											   (point-max) t)
							(match-beginning 0)))
			 (nick-end (if nick-beg
						   (progn (goto-char (match-end 0))
								  (search-forward " " nil t 1)
								  (point))
						 (point-min)))
			 (message (replace-regexp-in-string
					   "\n" " " (buffer-substring nick-end (point-max))))
			 (categories
			  (delq nil
					(list 'message
						  (when (null nickname) 'system)
						  (when (erc-query-buffer-p) 'query-buffer)
						  (when (or (erc-match-fool-p nickuserhost message)
									(erc-match-directed-at-fool-p message)) 'fool)
						  (when (erc-match-dangerous-host-p nickuserhost message)
							'dangerous-host)
						  (when (erc-match-current-nick-p nickuserhost message)
							'current-nick)
						  (when (erc-match-keyword-p nickuserhost message)
							'keyword)
						  (when (erc-match-pal-p nickuserhost message) 'pal))))
			 (notify-passes
			  (-keep
			   (-partial #'ercn-rule-passes-p
						 ercn-notify-rules nickname message)
			   categories))
			 (suppress-passes
			  (-keep
			   (-partial #'ercn-rule-passes-p
						 ercn-suppress-rules nickname message)
			   categories)))
		(when (and notify-passes
				   (null suppress-passes))
		  (run-hook-with-args 'ercn-notify-hook nickname message (current-buffer))))))
  (defalias 'ercn-match 'fn-ercn-match)



;;;; logging

  (defun fn-erc-show-chat-log ()
	(interactive)
	;; TODO: completing-read on erc-log-channels-directory
	)


;;;; Quick response

  (defvar +chat--erc-quick-response-buffer (make-ring 10))

  (defun +chat--erc-quick-response-on-message (nickname message buffer)
	"Called to update quick-response ring buffers."
	(set-text-properties 0 (length message) nil message)
	(ring-insert +chat--erc-quick-response-buffer
				 (list nickname message buffer)))

  (add-hook 'ercn-notify-hook '+chat--erc-quick-response-on-message)

  ;; (defun fn-erc-quick-response)

  (setq erc-server-list
		'(
		  ;; ("irc.freenode.net"
		  ;;  :port "6697"
		  ;;  :ssl t
		  ;;  :nick "phenoble")
		  ;; ("h2712310.stratoserver.net"
		  ;;  :port "6667"
		  ;;  :nick "fthevissen")
		  ))

  ;; -- connect on startup ----------


  ;; Overwrite znc-erc-connect to get password via auth-source
  )

;;; znc
(use-package znc
  :commands (+chat-irc-connect-znc)

  :config
  (require 'erc)
  (defun +chat-irc-connect-znc ()
	(interactive)
	(znc-all))

  (defun znc-erc-connect (endpoint)
	(with-endpoint endpoint
				   (message " -- znc connecting, have endpoint: %s" endpoint)
				   (let* ((buffer (znc-network-buffer-name slug))
						  (erc-fun (if ssl znc-erc-ssl-connector znc-erc-connector))
						  (auth (nth 0 (auth-source-search :host "h2712310.stratoserver.net"
														   :port 5000
														   :requires '(user secret))))
						  (password (funcall (plist-get auth :secret)))
						  (erc-args (progn
									  (when (null password)
										(error "znc-erc-connect: could not get password from .authinfo.gpg"))
									  `(:server ,host :port ,port
												:nick ,user :password ,(format "%s:%s" user password))))
						  (erc-buffer (apply erc-fun erc-args)))
					 (when (get-buffer buffer)
					   (znc-kill-buffer-always buffer))
					 (znc-set-name buffer erc-buffer)
					 (with-current-buffer erc-buffer
					   (rename-buffer buffer)))))

  )

(defun erc-history ()
  "Asks for string to look for in files, then opens
the best-matching and most-recent file."
  (interactive)
  (let* ((search-string (read-string "erc-history> "))
		 (files (directory-files erc-log-dir))
		 (best-match
		  (->> files
			   (-map (lambda (filename)
					   (cons (-
							  (or (car (flx-score filename search-string))
								  0)
							  (/ (float-time (file-attribute-modification-time
											  (file-attributes (concat
																erc-log-dir filename))))
								 86400))
							 filename)))
			   (--max-by (> (or (car it) 0) (or (car other) 0))))))
	(find-file (concat
				(f-slash erc-log-dir)
				(cdr best-match)))))

