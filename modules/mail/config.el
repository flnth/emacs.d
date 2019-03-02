;; modules/mail/config.el -*- lexical-binding: t; -*-

;; TODO:  thunk dir_local, dir_system... load on first use, then just use.

(load "mail/packages.el")
(load "mail/autoloads.el")

(require 'mm-decode)
(require 'gnus-icalendar)
(require 'gnus-cite)

(load "mail/+formatting.el")
(load "mail/+headers.el")
(load "mail/+imapd.el")
(load "mail/+query-rewrite.el")
(load "mail/+sidebar-extensions.el")
(load "mail/+updating.el")
(load "mail/+utils.el")

;;; mu4e

(add-to-list 'load-path (concat (getenv "DIR_LOCAL") "/share/emacs/site-lisp/mu4e"))

(use-package mu4e
  :after (mm-decode gnus-icalendar gnus-cite)
  ;; :commands mu4e ;; NOTE: don't need?
  :demand								; TODO might want to change
  :config (progn
;;;; general config
			;; TODO:  weird bug in mu4e / fill-article-cite somewhere:  error if buffer toes not exist
			(get-buffer-create "*Article*")

			(setq mu4e-hide-index-messages t ; hide info about indexing new messages
				  mu4e-headers-include-related nil ; don't show related messages in header view
				  mu4e-view-show-addresses t
				  mu4e-headers-auto-update nil ; nide idea, but loses loses position in list so disable
				  mu4e-user-mail-address-list (list "florian.thevissen@rwth-aachen.de" "florian.thevissen@indurad.com")
				  mu4e-view-show-images t
				  mu4e-view-image-max-width 800
				  mu4e-confirm-quit nil
				  mu4e-headers-date-format "%d.%m.%Y %H:%M" ; date format
				  mu4e-html2text-command "html2text -utf8 -width 72"

				  ;; configuration to send mail
				  mail-user-agent 'mu4e-user-agent
				  message-send-mail-function 'smtpmail-send-it
				  message-kill-buffer-on-exit t	; close after sending

				  ;; policies
				  mu4e-context-policy 'always-ask
				  mu4e-compose-context-policy 'pick-first

				  ;; encoding

				  ;; TODO:
				  ;; mu4e-completing-read-function
				  ;; (cond ((featurep! :completion ivy) #'ivy-completing-read)
				  ;;       ((featurep! :completion helm) #'completing-read)
				  ;;       (t #'ido-completing-read))

				  ;; mu4e-compose-signature 'message-signature
				  ;; mu4e-completing-read-function 'ivy-completing-read

				  ;; paths
				  mu4e-mu-binary (concat (getenv "DIR_LOCAL") "/bin/mu")
				  mu4e-maildirs-extension-count-command-format (concat mu4e-mu-binary " find %s --fields 'i' | wc -l")
				  mu4e-maildir (expand-file-name (concat (getenv "STACKROOT") "/.mail")))

;;;; encoding
			;; TODO: move somewhere else, it does not all belong here
			(set-keyboard-coding-system 'utf-8-unix)
			(set-language-environment 'utf-8)
			(set-default-coding-systems 'utf-8-unix)
			(setq message-send-coding-system 'utf-8-unix)
			(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
			(set-language-environment 'utf-8)
			(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
			(setq locale-coding-system 'utf-8)
			(set-default-coding-systems 'utf-8)
			(set-terminal-coding-system 'utf-8)
			(set-selection-coding-system
			 (if (eq system-type 'windows-nt)
				 'utf-16-le ;; https://rufflewind.com/2014-07-20/pasting-unicode-in-emacs-on-windows
			   'utf-8))
			(prefer-coding-system 'utf-8)
			(setq coding-system-for-write 'utf-8)

			(require 'mu4e-headers)


;;;; contexts
			(setq mu4e-contexts
				  ;; NOTE: convention:  context-names identical to mailfolder subdirectories
				  ;; -- rwth ------------------------------------------
				  `(,(when (string= (getenv "LOC") "home")
					   (make-mu4e-context
						:name "rwth"
						:enter-func (lambda () (mu4e-message "Entering rwth context"))
						:leave-func (lambda () (mu4e-message "Leaving rwth context"))
						:match-func (lambda (msg)
									  (when msg
										(mu4e-message-contact-field-matches
										 msg
										 :to "florian.thevissen@rwth-aachen.de")))
						:vars '((user-full-name			. "Florian Thevissen")
								(user-mail-address		. "florian.thevissen@rwth-aachen.de")
								(smtpmail-smtp-user		. "ft656695@rwth-aachen.de")
								(smtpmail-stream-type		. starttls)
								(smtpmail-smtp-server		. "mail.rwth-aachen.de")
								(smtpmail-smtp-service	. 587)
								(mu4e-compose-signature	. nil)
								(mu4e-drafts-folder		. "/rwth/Drafts")
								;; (mu4e-refile-folder )
								(mu4e-sent-folder		. "/rwth/Sent")
								(mu4e-trash-folder		. "/rwth/Trash")
								)))

					;; --indurad --------------------------------------
					,(when (string= (getenv "LOC") "work")
					   (make-mu4e-context
						:name "indurad"
						:enter-func (lambda () (mu4e-message "Entering indurad context"))
						:leave-func (lambda () (mu4e-message "Leaving indurad context"))
						:match-func (lambda (msg)
									  (when msg
										(mu4e-message-contact-field-matches
										 msg
										 :to "indurad.com")))
						:vars '((user-full-name			. "Florian Thevissen")
								(user-mail-address		. "florian.thevissen@indurad.com")
								(smtpmail-smtp-user		. "fthevissen")
								(smtpmail-stream-type 	. starttls)
								(smtpmail-smtp-server	    . "mail.indurad.com")
								(smtpmail-smtp-service	. 587)
								(mu4e-compose-signature	. nil)
								(mu4e-drafts-folder		. "/indurad/INBOX.Drafts")
								;; (mu4e-refile-folder )
								(mu4e-sent-folder		    . "/indurad/INBOX.Sent")
								(mu4e-trash-folder		. "/indurad/INBOX.Trash")
								)
						)) ;; TODO:  indurad signature
					;; --thevissen -------------------------------------
					,(when (string= (getenv "LOC") "home")
					   (make-mu4e-context
						:name "thevissen"
						:enter-func (lambda () (mu4e-message "Entering personal context"))
						:leave-func (lambda () (mu4e-message "Leaving personal context"))
						:match-func (lambda (msg)
									  (when msg
										(mu4e-message-contact-field-matches
										 msg
										 :to "florian-thevissen.de")))
						:vars '((user-full-name			. "Florian Thevissen")
								(user-mail-address		. "mail@florian-thevissen.de")
								(smtpmail-smtp-user		. "root@florian-thevissen.de")
								(smtpmail-stream-type 	. starttls)
								(smtpmail-smtp-server	    . "mail.florian-thevissen.de")
								(smtpmail-smtp-service	. 587)
								(mu4e-compose-signature	. nil)
								(mu4e-drafts-folder		. "/thevissen/Drafts")
								;; (mu4e-refile-folder )
								(mu4e-sent-folder		    . "/thevissen/Sent")
								(mu4e-trash-folder		. "/thevissen/Trash")
								)
						)) ;; TODO:  indurad signature
					))

;;;; icons
			(setq use-fancy-chars t)
			(setq mu4e-headers-draft-mark		'("D" . "⚒")
				  mu4e-headers-flagged-mark	'("F" . "")
				  mu4e-headers-new-mark		'("N" . "✱")
				  mu4e-headers-passed-mark	'("P" . "❯")
				  mu4e-headers-replied-mark	'("R" . "❮")
				  mu4e-headers-seen-mark		'("S" . "✔")
				  mu4e-headers-trashed-mark	'("T" . "⏚")
				  mu4e-headers-attach-mark	'("a" . "⚓")
				  mu4e-headers-encrypted-mark	'("x" . "⚴")
				  mu4e-headers-signed-mark	'("s" . "☡")
				  mu4e-headers-unread-mark	'("u" . " "))

			(setq mu4e-headers-thread-child-prefix 		 '("├> "  . "▪ ")
				  mu4e-headers-thread-last-child-prefix	 '(" └> " . "▫ ")
				  mu4e-headers-thread-connection-prefix	 '("│"	  . "│")
				  mu4e-headers-thread-orphan-prefix		 '("┬> "  . "▻ ")
				  mu4e-headers-thread-single-orphan-prefix '(" ─> " . " ─> "))

			;; always show text/plain parts
			(add-to-list 'mm-discouraged-alternatives "text/html")
			(add-to-list 'mm-discouraged-alternatives "text/richtext")
			(setq mm-automatic-display (remove "text/html" mm-automatic-display)
				  mu4e-view-prefer-html nil
				  mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum)

			;; experimental:  buttons for when a message contains calendar invitations
			(gnus-icalendar-setup)
			(mu4e~request-contacts)

;;;; hooks
			;; TODO: find better solution than this?
			;;       , though I am not sure if I want to not have all this at one point only

			(defun fn--mu4e-view-mode-hook ()
			  (interactive)
			  (tabbar-local-mode 1)
			  ;; (font-lock-mode 1)
			  (highlight-parentheses-mode -1)
			  (scroll-bar-mode -1)
			  (yascroll-bar-mode -1)
			  ;; (visual-line-mode)
			  (setq fill-column 81)
			  (visual-fill-column-mode)
			  )
			(add-hook 'mu4e-view-mode-hook #'fn--mu4e-view-mode-hook)

			(defun fn--mu4e-compose-mode-hook ()
			  (interactive)
			  (company-mode)
			  (html2text)
			  (setq fill-column 81)
			  (visual-fill-column-mode)
			  (gnus-message-citation-mode)
			  )
			(add-hook 'mu4e-compose-mode-hook #'fn--mu4e-compose-mode-hook)

			(defun fn--mu4e-headers-mode-hook ()
			  (interactive)
			  (yascroll-bar-mode -1)
			  )
			(add-hook 'mu4e-headers-mode-hook #'fn--mu4e-headers-mode-hook)


;;;; keybindings (howto with use-package?)
			(evil-define-key '(normal insert evilified) mu4e-headers-mode-map
			  "q" #'fn-mu4e-headers-quit-buffer
			  (kbd "C-S-h") #'mu4e-headers-query-prev
			  (kbd "C-S-l") #'mu4e-headers-query-next
			  (kbd "/") #'mu4e-headers-search-narrow
			  (kbd "gr") #'mu4e-headers-rerun-search
			  (kbd "gb") #'fn-ido-switch-buffer
			  (kbd "d") #'(lambda ()
							"Move message at point, or messages in region, to
								   trash and schedule update."
							(interactive)
							(if (not (use-region-p))
								;; single message
								(fn-mu4e-move-to-trash-at-point)
							  ;; region
							  (save-excursion
								(let ((cant-go-further) (eor (region-end)))
								  (goto-char (region-beginning))
								  (while (and (< (point) eor) (not cant-go-further))
									(fn-mu4e-move-to-trash-at-point)
									(setq cant-go-further (not (mu4e-headers-next)))))))

							(evil-exit-visual-state)
							(fn-mu4e-on-execute-hook nil nil)
							(sleep-for 0.2) ; TODO: removing "INBOX." prefixes with this does not work, fix
							(fn-mu4e-fix-maildir-names)
							(mail-sidebar-buffer--refresh))
			  (kbd "RET") 'mu4e-headers-view-message
			  )

			(evil-define-key '(normal insert evilified) mu4e-view-mode-map
			  (kbd "C-S-h") #'mu4e-view-headers-prev
			  (kbd "C-S-l") #'mu4e-view-headers-next
			  (kbd "C-h") #'html2text
			  (kbd "}") #'evil-forward-paragraph
			  (kbd "{") #'evil-backward-paragraph
			  )
			(spacemacs/set-leader-keys (kbd "ss") #'mu4e-headers-search)
			)
  )

;;; mu4e-maildirs-extension

(use-package mu4e-maildirs-extension
  :after mu4e
  :config (progn
			(mu4e-maildirs-extension)
			(setq mu4e-maildirs-extension-title nil ; string shown before maildirs
				  mu4e-maildirs-extension-updating-string "" ; another string shown somewhere
				  mu4e-maildirs-extension-use-bookmarks t
				  mu4e-maildirs-extension-buffer-name "*Mail*"
				  mu4e-maildirs-extension-insert-before-str "\n\t[c] Compose a message"
				  mu4e-maildirs-extension-maildir-expanded-prefix ""
				  mu4e-maildirs-extension-maildir-default-prefix "")

;;;; automatic population of maildirs
			(setq fn-mu4e-ignored-maildirs '("/rwth/INBOX\.+"
											 "Archives*"
											 "Drafts"
											 "Aufgaben"
											 "Entw\&Apw\-rfe"
											 "Gel\&APY\-schte Elemente"
											 "Postausgang"
											 "Kontakte"
											 "Kalender"
											 "Journal"
											 "Notizen"
											 "Junk-E-Mail"
											 "/\\."
											 "/\\.\\."
											 "Gesendete Elemente")

				  fn-mu4e-front-maildirs '("/rwth/INBOX"
										   "/indurad/INBOX"
										   "/fthevissen/INBOX")
				  )
			(fn-mu4e-add-maildirs)
			))

;;; sidebar

(use-package sidebar
  :load-path (lambda () (concat (getenv "DIR_SYSTEM") "/emacs/modules/mail"))
  :after  (mu4e mu4e-maildirs-extension cl-lib dash ht)
  :config
  (setq mail-sidebar-window-width 35
		mail-sidebar-autorefresh nil)

  ;; TODO: sort use-package <-> spacemacs stuff, if possible
  (spacemacs/set-leader-keys "am" 'mail-sidebar-toggle )

  ;; no evil in mu4e-loading buffer
  (add-to-list 'evil-buffer-regexps '("^ \\*mu4e-loading\\*"))

;;;; keybindings
  (evil-define-key '(normal insert evilified) mail-sidebar-mode-map
	"gr" #'fn-mu4e-update-sidebar
	(kbd "TAB") #'mu4e-maildirs-extension-toggle-maildir-at-point)
  )


;;; outlook-style

(use-package outlook-style
  :after (mu4e)
  :init
  (add-to-list 'load-path (concat (getenv "DIR_SYSTEM") "/emacs/gnus-outlook-style/"))
  :config
  (setq outlook-style-format-helper-location
		(concat (getenv "DIR_SYSTEM")
				"/emacs/gnus-outlook-style/format-html-email/format_quoted_mail")
		outlook-style-post-active t)
  (outlook-style-setup-mu4e)

;;;; signatures
  (add-hook 'outlook-style-post-plaintext-hook
			(lambda () (fn-insert-signature (concat (getenv "DIR_SYSTEM") "/mail/sig-plain"))))

  (add-hook 'outlook-style-post-transform-hook
			(lambda () (fn-insert-signature (concat (getenv "DIR_SYSTEM") "/mail/sig-html")))))
