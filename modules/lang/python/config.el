;;; modules/lang/python/config.el                        -*- lexical-binding: t; -*-


(use-package python
  :after lsp-mode
  :bind (
		 :map inferior-python-mode-map
		 ("C-n" . comint-next-input)
		 ("C-p" . comint-previous-input)
		 ("C-j" . newline-and-indent)
		 :map python-mode-map
		 ("C-p" . (lambda () (interactive) (goto-closest-imenu-item -1)))
		 ("C-n" . (lambda () (interactive) (goto-closest-imenu-item 1)))
		 ("M-n" . outline-next-heading)
		 ("M-p" . outline-previous-heading)
		 )
  :config
  (use-package lang/python/folding
	)

  (let ((loc (getenv "LOC")))

	(setq python-shell-interpreter-args "--simple-prompt"
		  python-shell-interpreter (pcase loc
									 ("work" "/home/fthevissen/localstorage/.dotfiles/bin/ipython")
									 ("home" "/home/fthevissen/software/anaconda/bin/ipython")
									 (_ (error "Cannot set python-shell-interpreter.")))
		  )
	)

  (evil-define-key '(normal visual insert) python-mode-map
	(kbd "C-p")  '(lambda () (interactive) (goto-closest-imenu-item -1))
	(kbd "C-n")  '(lambda () (interactive) (goto-closest-imenu-item 1))
	(kbd "<C-tab>") 'fn-hs-toggle-hiding
	(kbd "<C-M-tab>") '+py-toggle-fold-surrounding-context
	(kbd "M-RET") #'outshine-insert-heading
	;; -- lsp
	(kbd "M-.") #'lsp-find-definition
	(kbd "C-k") #'lsp-describe-thing-at-point
	(kbd "C-_") #'+lsp-trigger-ui-doc
	(kbd "C-/") #'+lsp-trigger-ui-doc
	)


  ;; TODO: needed? find other solution?
  ;; (local-set-key "\M-z" 'python-send)
  ;; (local-set-key (kbd "<f54>") 'python-execute-file) ;; terminal
  ;; (local-set-key (kbd "<M-f6>") 'python-execute-file)

  ;; python-mode config
  (defun +python-mode-config ()
	;; TODO: working? necessary?
	(setq tab-width 4)		  ;; but please 4 spaces for them
	(setq python-tab-width 4) ;; but please 4 spaces for them
	(imenu--make-index-alist t)
	(setq company-backends nil)
	;; (setq completion-at-point-functions nil)
	)
  (add-hook #'python-mode-hook #'+python-mode-config)


  ;; lsp-python-enable
  (defun +lsp-python-enable ()
	(when (and (buffer-file-name)
			   (setq-local lsp--buffer-workspaces (or (lsp--try-open-in-library-workspace)
													  (lsp--try-project-root-workspaces nil))))
	  (message "lsp-python-enable!")
	  (lsp-mode 1)
	  (lsp-ui-mode 1)
	  (lsp-ui-sideline-mode -1)
	  (company-mode 1)
	  ;; wait a little until configuring company. Spacemacs interfere hiddenly.
	  (run-with-timer 0.5 nil #'(lambda ()
								  (make-local-variable 'company-backends)
								  ;; (add-to-list 'company-backends 'company-lsp)
								  (setq company-backends '(company-lsp))))
	  ))
  (add-hook #'python-mode-hook #'+lsp-python-enable)


  (remove-hook #'python-mode-hook #'anaconda-mode)
  (remove-hook #'python-mode-hook #'spacemacs//pyvenv-mode-set-local-virtualenv)
  (remove-hook #'python-mode-hook #'spacemacs//python-default)
  (remove-hook #'python-mode-hook #'spacemacs//python-setup-backend)
  (remove-hook #'python-mode-hook #'importmagic-mode)
  (remove-hook #'python-mode-hook #'spacemacs//init-jump-handlers-python-mode)

  (defun +inferior-python-mode-config ()
	(setq completion-at-point-functions nil)
	(setq header-line-format nil)
	(vi-tilde-fringe-mode -1))
  (add-hook 'inferior-python-mode-hook #'+inferior-python-mode-config)


  ;; (setq company-global-modes '(not inferior-python-mode))


  ;; (remove-hook #'inferior-python-mode-hook #'spacemacs//inferior-python-setup-hook)
  ;; (remove-hook #'inferior-python-mode-hook #'spacemacs//init-company-inferior-python-mode)
  ;; (remove-hook #'inferior-python-mode-hook #'spacemacs//init-company-vars-inferior-python-mode)

  ;; features
  ;;(spacemacs/toggle-automatic-symbol-highlight-on)

  ;; TODO: necessary:
  ;; (add-hook 'prog-mode-hook (lambda ()
  ;;                             (setq truncate-lines t)))

  ;; TODO: do call somewhere for initialization?
  ;; (python-shell-make-comint (python-shell-calculate-command)
  ;; 							(python-shell-get-process-name nil) nil)

  ;; TODO: necessary?
  ;; (evil-define-key '(normal insert evilified) inferior-python-mode-map (kbd "C-j") nil)

  ;; (define-key inferior-python-mode-map (kbd "C-n") #'comint-next-input)
  ;; (define-key inferior-python-mode-map (kbd "C-p") #'comint-previous-input)
  ;; (define-key inferior-python-mode-map (kbd "C-j") #'newline-and-indent)

  )

;; TODO: used?
;; (defun fn-python-current-directory ()
;;   (interactive)
;;   (python-shell-send-string (concat "cd " default-directory)))

;;  my own custom isend-mode
;; (defun python-send ()
;;   (interactive)
;;   (if (region-active-p)
;; 	  (progn
;; 		(python-shell-send-region (region-beginning) (region-end))
;; 		(deactivate-mark))
;; 	(progn
;; 	  (mwim-beginning-of-code-or-line-or-comment)
;; 	  (set-mark (point))
;; 	  (mwim-end-of-code)
;; 	  (activate-mark)
;; 	  (setq deactivate-mark nil)
;; 	  (python-shell-send-region (region-beginning) (region-end))
;; 	  (deactivate-mark))))

;; execute current file on M-F6
;; TODO: have a mode where ipython is not restarted (is alot faster!)
;; (defun python-execute-file ()
;;   "Run the current file in a new ipython process within the
;; *Python* buffer."
;;   (interactive)
;;   (save-buffer)
;;   (if (get-buffer-process "*Python*")
;; 	  (kill-process "Python"))
;;   (sleep-for 0 50)
;;   (setq working-directory (file-name-directory (buffer-file-name)))
;;   (with-current-buffer (get-buffer-create "*Python*")
;; 	(cd working-directory))
;;   (run-python (python-shell-calculate-command) nil nil)
;;   (python-shell-send-string (concat "%run  "
;; 									(buffer-file-name))))

;; ---------------------------------------------------------




