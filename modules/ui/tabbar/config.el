;; modules/ui/tabbar/config.el    -*- lexical-binding: t; -*-


;;;;; tabbar
(require 'aquamacs-tabbar)

;; -> module tabbar

;; (define-key evil-motion-state-map "gb" #'fn-ido-switch-buffer)
;; (define-key messages-buffer-mode-map "gb" #'fn-ido-switch-buffer)

;; find-file
(define-key evil-motion-state-map (kbd "C-x C-f") #'find-file)
(global-set-key (kbd "C-x C-f") #'find-file)
;; remove find-file from counsel-mode, use normal one instead
;; (background: I don't want to just disable counsel-mode at the moment, so mod it.)
(setf (alist-get 'find-file (alist-get 'remap (cdr counsel-mode-map))) 'find-file)

;; M-x
;; (evil-define-key '(visual normal evilified) org-agenda-mode-map "gb" #'fn-ido-switch-buffer)
;; (define-key org-agenda-keymap (kbd "M-x") #'fn-M-x)
;; (define-key org-agenda-keymap (kbd "M-X") #'counsel-M-x)

(defun fn-close-window (arg)
  "Close current tab, then window, then window configuration.
  Close window when prefix argument is given. Ignores sidebar
  windows."
  (interactive "P")
  (let ((num-buffers-in-tabbar
		 (length (cdr (assq (window-number (selected-window))
							tabbar-window-alist))))
		(num-windows-in-frame (length (remove nil (cl-loop for window in (window-list) collect
														   (with-selected-window window
															 (if (window-parameter (selected-window) 'window-side) nil t))))))
		(num-eyebrowse-configurations (length (eyebrowse--get 'window-configs (selected-frame))))
		)
	;; (message "clos-window, buf-tab,win-frame: %s, %s" num-buffers-in-tabbar num-windows-in-frame)
	(cond ((and (> num-buffers-in-tabbar 1) (not arg))
		   (tabbar-close-tab))
		  ((eq 1 num-windows-in-frame)
		   (if (eq 1 num-eyebrowse-configurations)
			   (delete-frame)
			 (eyebrowse-close-window-config)))
		  (t
		   (delete-window)))))

(define-key evil-normal-state-map (kbd "C-t") 'tabbar-new-tab)
(define-key evil-normal-state-map (kbd "C-w") 'fn-close-window)
(define-key evil-normal-state-map (kbd "<C-prior>") 'tabbar-backward-tab)
(define-key evil-normal-state-map (kbd "<C-next>") 'tabbar-forward-tab)

(global-set-key (kbd "C-t") 'tabbar-new-tab)
(global-set-key (kbd "C-w") 'fn-close-window)
(global-set-key (kbd "<C-prior>") 'tabbar-forward-tab)
(global-set-key (kbd "<C-next>") 'tabbar-backward-tab)

(add-to-load-path (concat dir_emacs "/packages/tabbar") )

(tabbar-mode 1)


;; overwrite the formatting of a single tab
(defun tabbar-line-tab (tab)
  "Return the display representation of tab TAB.
That is, a propertized string used as an `header-line-format' template
element.
Call `tabbar-tab-label-function' to obtain a label for TAB."
  (let* ((selected-p (tabbar-selected-p tab (tabbar-current-tabset)))
		 (close-button-image (tabbar-find-image tabbar-close-tab-button))
		 (mouse-face (if selected-p
						 'tabbar-selected-highlight
					   'tabbar-unselected-highlight))

		 (text-face (if selected-p
						'tabbar-selected
					  'tabbar-unselected))
		 (display-label
		  (propertize (if tabbar-tab-label-function
						  (funcall tabbar-tab-label-function tab)
						tab)
					  'tabbar-tab tab
					  'local-map (tabbar-make-tab-keymap tab)
					  ;;	  'help-echo 'tabbar-help-on-tab ;; no help echo: it's redundant
					  'mouse-face mouse-face
					  'face (cond ((and selected-p
										(buffer-modified-p (tabbar-tab-value tab)))
								   'tabbar-selected-modified)
								  ((and (not selected-p)
										(buffer-modified-p (tabbar-tab-value tab)))
								   'tabbar-unselected-modified)
								  ((and selected-p
										(not (buffer-modified-p (tabbar-tab-value tab))))
								   'tabbar-selected)
								  (t 'tabbar-unselected))
					  'pointer 'arrow))
		 )
	(concat display-label)))

;; TODO disabled, performance problems
(defun tabbar-line-tab (tab) nil)

;; overwrite formatting of the whole tabbar
;; TODO: rewrite this algo, it's crap (!)
(defun tabbar-line-format (tabset)
  "Return the `header-line-format' value to display TABSET."
  (let* ((sel (tabbar-selected-tab tabset))
		 (tabs (tabbar-view tabset))
		 (padcolor (tabbar-background-color))
		 (noscroll t)
		 (tabbar-line-tabs (tabbar-tabs tabset))
		 atsel elts scrolled)

	(if (eq 1 (length tabs))
		""

	  (let ((left-header-line (+header-line-format-left 50))
			(right-header-line (+header-line-format-right))
			)
		;; Make sure we're showing as many tabs as possible.  If we're
		;; not showing the 1st tab, and we're not overflowing the tab
		;; bar, then scroll backward.  If this leads to overflowing the
		;; tab bar, scroll forward 1 at the end.
		(while (and (> (get tabset 'start) 0)
					(not (tabbar-check-overflow tabset left-header-line right-header-line)))
		  (tabbar-scroll tabset -1)
		  (setq scrolled t))

		;; if we scrolled until the tabbar overflowed, we went too far.
		;; Back up 1 slot.
		(when (and scrolled (tabbar-check-overflow tabset left-header-line right-header-line))
		  (tabbar-scroll tabset 1))

		(when (or (> (tabbar-start tabset) 0)
				  (tabbar-check-overflow tabset left-header-line right-header-line))
		  ;; not all tabs fit -- include scroll buttons
		  (setq noscroll nil))

		;; Track the selected tab to ensure it is always visible.
		(when tabbar--track-selected
		  (while (not (memq sel tabs))
			(tabbar-scroll tabset -1)
			(setq tabs (tabbar-view tabset)))
		  (while (and tabs (not atsel))
			(setq elts  (cons (tabbar-line-tab (car tabs)) elts)
				  atsel (eq (car tabs) sel)
				  tabs  (cdr tabs)))
		  (setq elts (nreverse elts))

		  ;; At this point the selected tab is the last elt in ELTS.
		  ;; Scroll TABSET and ELTS until the selected tab becomes
		  ;; visible.

		  ;; because of the post-hoc scrolling,
		  ;; we can't determine the line index beforehand

		  (with-temp-buffer
			(let ((truncate-partial-width-windows nil)
				  (inhibit-modification-hooks t)
				  deactivate-mark ;; Prevent deactivation of the mark!
				  start)
			  (setq truncate-lines nil
					buffer-undo-list t)

			  ;; insert left,right parts
			  (insert left-header-line)
			  (insert right-header-line)

			  (setq start (point))
			  (while (and (cdr elts) ;; Always show the selected tab!
						  (progn
							(delete-region start (point-max))
							(goto-char (point-max))
							(apply 'insert elts)
							(goto-char (point-min))
							(> (vertical-motion 1) 0)))
				(tabbar-scroll tabset 1)
				(setq elts (cdr elts)))))

		  (setq elts (nreverse elts))
		  (setq tabbar--track-selected nil))

		;; Format remaining tabs.
		(while tabs
		  (setq elts (cons (tabbar-line-tab (car tabs)) elts)
				tabs (cdr tabs)))
		;; Cache and return the new tab bar.
		(tabbar-set-template
		 tabset
		 (list
		  left-header-line
		  (nreverse elts)
		  right-header-line
		  ))))))

;; TODO disabled, performance problems
(defun tabbar-line-format (tabset) nil )

;; overwrite overflow check
(defun tabbar-check-overflow (tabset &optional noscroll left-aligned right-aligned)
  "Return t if the current tabbar is longer than the header line.
If NOSCROLL is non-nil, exclude the tabbar-scroll buttons in the
check."
  (let ((tabs (tabbar-view tabset))
		elts)
	(while tabs
	  (setq elts  (cons (tabbar-line-tab (car tabs)) elts)
			tabs  (cdr tabs)))
	(setq elts (nreverse elts))
	(with-temp-buffer
	  (let ((truncate-partial-width-windows nil)
			(inhibit-modification-hooks t)
			deactivate-mark ;; Prevent deactivation of the mark!
			start)
		(setq truncate-lines nil
			  buffer-undo-list t)
		(insert (or left-aligned ""))
		(setq start (point))
		(delete-region start (point-max))
		(goto-char (point-max))
		(apply 'insert elts)
		(insert (or right-aligned ""))
		(goto-char (point-min))
		(let* ((vert (vertical-motion 1))
			   (ret (> vert 0)))
		  ret)))))


;; redefine tab labels
(defun tabbar-window-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((filename (ignore-errors (buffer-file-name (tabbar-tab-value tab)))))
	(concat (format " %s"
					(if filename (f-filename filename)
					  (tabbar-tab-value tab)))
			(if (buffer-modified-p (tabbar-tab-value tab))
				"* " " "))))

(add-hook 'helpful-mode-hook 'tabbar-local-mode)
(add-hook 'inferior-emacs-lisp-mode-hook 'tabbar-local-mode)
(add-hook 'inferior-python-mode 'tabbar-local-mode)


;;;; auto-close tabs

(defun fn-tabbar-on-quit-window (&optional kill window)
  (interactive)
  (when (or (null window)
			(eq window (selected-window)))
	;; can't be sure the window is the current one --> better do nothing.
	(tabbar-close-tab)))

;; :after for popup-like
(advice-add 'quit-window :after 'fn-tabbar-on-quit-window)

(defun fn-tabbar-on-bury-buffer (&optional buffer-or-name)
  (let ((buf (if (stringp buffer-or-name)
				 (get-buffer buffer-or-name)
			   buffer-or-name)))
	(when (or (null buffer-or-name)
			  (eq buf (current-buffer)))
	  (tabbar-close-tab))))

;; :after for popup-like
;; (advice-add 'bury-buffer :after 'fn-tabbar-on-bury-buffer)
;; (advice-remove 'bury-buffer'fn-tabbar-on-bury-buffer)

;;;; open buffer in new tab

(defun fn-tabbar-switch-to-buffer (buffer-or-name &optional norecord force-same-window )
  (tabbar-new-tab)
  (switch-to-buffer buffer-or-name norecord force-same-window))

(defun fn-tabbar-pop-to-buffer (buffer-or-name &optional action norecord)
  (tabbar-new-tab)
  (pop-to-buffer buffer-or-name action norecord))
