;;; modules/lang/lisp/hexpand.el    -*- lexical-binding: t; -*-

;; ideas:
;; - a hydra for cycling through expansion state
;; - use lower-level quick-peek functions
;; - show the popup after the macro definition, if its end is visible, and this
;;   would cause the window to be shown closer to the window's top than
;;   otherwise.

(defhydra expansion ()
  "^Form Expand^"
  ("h" (message "previous expansion"))
  ("l" (message "next expansion"))
  )

(defun hexpand--get-defun-name ()
  "Exracts the name of the function at point. "
  (interactive)
  (save-excursion
	(narrow-to-defun)
	(unwind-protect
		(let (name)
		  (beginning-of-defun)
		  (if (re-search-forward "\\(defmacro\\|defun\\)\s+\\([^\s|^\t]*\\)\s*" nil t)
			  (progn
				(setf name (match-string 2))
				(set-text-properties 0 (length name) nil name)
				name)
			(error "Could not extract name of defun or macro at point.")))
	  (widen))))

(setq hexpand--macro-expand-history '())

(defun hexpand--prompt-for-macro-expand (name)
  "Prompts for the expansion of macro NAME, and returns them as a
  list." (let* ((name (hexpand--get-defun-name))
		 (prompt (concat "macroexpand, args ["
						 (propertize name 'face
									 '(:foreground "#ac443f"))
						 "]> ")))
		   (read (concat
				  "("
				  (read-string prompt
							   (car hexpand--macro-expand-history)
							   'hexpand--macro-expand-history )
				  ")"))))

;; (defun hexpand-expand-macro-at-point ()
;;   "Returns the expansion of the macro at point as
;; string."
;;   (let (out)
;; 	(save-mark-and-excursion
;; 	  (end-of-defun)
;; 	  (set-mark (point))
;; 	  (pp-macroexpand-last-sexp t)
;; 	  (activate-mark)
;; 	  (setf out (buffer-substring (region-beginning) (region-end)))
;; 	  (delete-region (region-beginning) (region-end)))
;; 	out))

(defun hexpand-expand-macro (name args)
  "Returns the expansion of the macro at point as
string."
  (let (raw-string)
	(save-mark-and-excursion
	  (end-of-defun)
	  (set-mark (point))
	  ;; (pp-macroexpand-last-sexp t)
	  ;; (insert (format "%s"
	  ;; 				  (macroexpand `(,name ,@args) )))
	  (cl-prettyexpand `(,name ,@args) nil)

	  (activate-mark)
	  (setf raw-string (buffer-substring (region-beginning) (region-end)))
	  (delete-region (region-beginning) (region-end)))
	(with-temp-buffer
	  (cl-prettyexpand `(,name ,@args) nil)
	  (insert raw-string)
	  (delay-mode-hooks (emacs-lisp-mode))
	  (font-lock-fontify-region (point-min) (point-max))
	  (buffer-string))
	))

(defun hexpand-extract-macro-body (ex-macro-string)
  "Extract the body of the expanded macro definition in the
  current buffer, and return the string."
  ;; NOTE: relies on smartparens
  (with-current-buffer (get-buffer-create "*Pp Macroexpand Output*")
	(erase-buffer)
	(goto-char (point-min))
	(insert ex-macro-string)
	(goto-char (point-min))
	(if (re-search-forward "^[\s\\|\t]+(cl-block" nil t)
		(progn
		  (evil-next-visual-line)
		  (end-of-line)
		  (sp-narrow-to-sexp nil)
		  (delay-mode-hooks (emacs-lisp-mode))
		  (font-lock-fontify-region (point-min) (point-max))
		  (buffer-substring (point-min) (point-max)))
	  (error "Could not extract macro body."))))

(defun hexpand-quick-peek-expand-macro ()
  "Expand macro at point and show in quick-peek buffer."
  (interactive)

  (eval-defun nil)
  (save-mark-and-excursion
	(let* ((macro-name (hexpand--get-defun-name))
		   (expansion-args (hexpand--prompt-for-macro-expand macro-name))
		   (pp-macro-expansion (with-temp-buffer
								 (erase-buffer)
								 (cl-prettyexpand `(,(read macro-name)
													,@expansion-args))
								 (let ((beg (progn
											  (goto-char (point-min))
											  (re-search-forward "[^\s\\|^\t\\|^\n]")
											  (max (- (point) 1) (point-min))
											  ))
									   (end (progn
											  (goto-char (point-max))
											  (re-search-backward "[^\s\\|^\t\\|^\n]")
											  ;; (+ (point) 1)
											  (min (+ (point) 1) (point-max))
											  )))
								   (emacs-lisp-mode)
								   (font-lock-fontify-region beg end)
								   (concat
									(propertize
									 (format "%s:" `(,(read macro-name)
													 ,@expansion-args))
									 'face '(:foreground "gray50" :underline t)
									 )
									(propertize " " 'display `(space . (:align-to
																		,(- (window-width) 3))))
									"1"
									"\n\n"
									(buffer-substring beg end)
									"\n"
									))))
		   (num-lines (+ 2 (s-count-matches "\n" pp-macro-expansion)))
		   (show-position (save-excursion
							(goto-char (window-end))
							(evil-previous-visual-line (min (window-height) num-lines) )
							(point)
							))
		   )
	  (quick-peek-show pp-macro-expansion show-position nil 100)
	  (cl-labels (
	  			  (hide
	  			   ()
				   (mapcar #'(lambda (win)
				   			   (with-selected-window win
				   				 (quick-peek-hide)))
				   		   (window-list))
	  			   (remove-hook 'post-command-hook #'hide)
				   )
				  (install-hook
	  			   ()
	  			   (add-hook 'post-command-hook #'hide))
				  )
	  	(run-with-timer 0.2 nil #'install-hook))
	  )
	)


  ;; (display-buffer-in-side-window (get-buffer-create "*Pp Macroexpand Output*")
  ;; 								 '((side . right)))
  ;; (pop-to-buffer "*Pp Macroexpand Output*")
  )

(provide 'lang/lisp/hexpand)
