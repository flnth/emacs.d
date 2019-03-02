;; modules/feature/lsp/config.el    -*- lexical-binding: t; -*-

(use-package lsp-mode
  :config

  ;; change nil to 't to enable logging of packets between emacs and the LS
  ;; this was invaluable for debugging communication with the MS Python Language Server
  ;; and comparing this with what vs.code is doing
  (setq lsp-print-io nil
		lsp-enable-snippet t  			; ?
		lsp-enable-xref t				; ?
		lsp-enable-indentation nil		; ?
		lsp-enable-on-type-formatting t ; ?
		lsp-before-save-edits nil
		lsp-hover-enabled nil			; does nothing?
		lsp-signature-enabled t			; for eldoc
		lsp-auto-guess-root t			; auto-guess root
		;; -32800 default, -32603: internal error, -32002: server not initialized
		lsp--silent-errors '(-32800 -32603 -32002)
		;; company-transformers nil     ; ?
		;; company-lsp-async t          ; ?
		;; company-lsp-cache-candidates nil ; ?
		;; cquery-extra-init-params '(:completion (:detailedLabel t)) ; for better formatting?
		)

  ;; TODO:  lsp-describe-thing-at-point @ ctrl-k for c-mode / c++-mode, when lsp-mode enabled?
  ;;  -> manage key-bindings dependent on lsp status...?

  (use-package lsp-ui
	:ensure t
	:config
	(setq lsp-ui-sideline-ignore-duplicate t)
	;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)

	)

  ;; make sure we have lsp-imenu everywhere we have LSP
  ;; (require 'lsp-imenu)
  ;; (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

  ;; install LSP company backend for LSP-driven completion
  (use-package company-lsp
	:ensure t
	;; :config
	;; (push 'company-lsp company-backends)
	)

  (use-package lsp-python-ms
  	:after lsp-mode
    :config

    ;; for dev build of language server
    (setq lsp-python-ms-dir
          (expand-file-name "/home/fthevissen/software/python-language-server/output/bin/Release/"))
    ;; for executable of language server
    (setq lsp-python-ms-executable
          "/home/fthevissen/software/local/bin/Microsoft.Python.LanguageServer")
  	)


  ;; (require 'lsp-python-ms)

  (defun +lsp-trigger-ui-doc ()
	"Manually triggers rendering of ui documentation."
	(interactive)

	;; install hook for display
	(add-hook 'lsp-on-hover-hook 'lsp-ui-doc--on-hover nil t)
	(let* ((lexical-binding t)
		   (request-id (cl-incf lsp-hover-request-id))
		   signature-response hover-response)
	  (if (lsp--capability "signatureHelpProvider")
		  (lsp-request-async
		   "textDocument/signatureHelp"
		   (lsp--text-document-position-params)
		   (lambda (signature-help)
			 (when (eq request-id lsp-hover-request-id)
			   (lsp--display-signature-or-hover
				(setf signature-response (or signature-help :empty))
				hover-response))))
		(setf signature-response :empty))
	  (if (lsp--capability "hoverProvider")
		  (lsp-request-async
		   "textDocument/hover"
		   (lsp--text-document-position-params)
		   (lambda (hover)
			 (when (eq request-id lsp-hover-request-id)
			   (setf hover-response (or hover :empty))
			   (lsp--display-signature-or-hover signature-response hover-response))))
		(setf hover-response :empty)))

	(cl-labels ((clear-display ()		; clear display on any command
							   (lsp-ui-doc--display 'nop "")
							   (run-with-timer 0.5 nil (lambda ()
							   							 (remove-hook 'post-command-hook #'clear-display)))
							   ))

	  (run-with-timer 0.2 nil  ; remove display hook after short period of time,
					  #'(lambda ()	   ; clear display on first command received
						  (add-hook 'post-command-hook #'clear-display)
						  (remove-hook 'lsp-on-hover-hook 'lsp-ui-doc--on-hover t)) )
	  )

	)

  ;; fix child-frame font size
  (defun lsp-ui-doc--render-buffer (string symbol)
	"Set the buffer with STRING."
	(lsp-ui-doc--with-buffer
	 (erase-buffer)
	 (let ((inline-p (lsp-ui-doc--inline-p))
		   (fontsize (face-attribute 'default :height (selected-frame)))
		   )
       (insert (concat (unless inline-p (propertize "\n" 'face '(:height 0.2)))
                       (-> (replace-regexp-in-string "`\\([\n]+\\)" ""
													 (propertize  string 'face `(:height ,fontsize)))
                           (string-trim-right))
                       (unless inline-p (propertize "\n\n" 'face '(:height 0.3)))))
	   (lsp-ui-doc--make-clickable-link)
	   (setq-local face-remapping-alist `((header-line lsp-ui-doc-header)))
	   (setq-local window-min-height 1)
	   (setq header-line-format (when lsp-ui-doc-header (propertize  (concat " " symbol) 'face `(:height ,(+ fontsize 20))))
			 mode-line-format nil
			 cursor-type nil))
	 ))


  ;; fix lsp-ui-sideline-mode
  (define-minor-mode lsp-ui-sideline-mode
	"Minor mode for showing information of current line."
	:init-value nil
	:group lsp-ui-sideline
	(cond
	 (lsp-ui-sideline-mode
      (add-hook 'post-command-hook 'lsp-ui-sideline nil t)
      (advice-add 'company-pseudo-tooltip-frontend :before 'lsp-ui-sideline--hide-before-company)
      (add-hook 'lsp-after-diagnostics-hook 'lsp-ui-sideline--diagnostics-changed nil t)
      (when lsp-ui-sideline-show-diagnostics
		(setq-local flycheck-display-errors-function nil)))
	 (t
      (setq lsp-ui-sideline--tag nil)
      (advice-remove 'company-pseudo-tooltip-frontend 'lsp-ui-sideline--hide-before-company)
      (lsp-ui-sideline--delete-ov)
      (remove-hook 'lsp-after-diagnostics-hook 'lsp-ui-sideline--diagnostics-changed t)
      (remove-hook 'post-command-hook 'lsp-ui-sideline t)
      (when lsp-ui-sideline-show-diagnostics
		(kill-local-variable 'flycheck-display-errors-function)))))

  (define-minor-mode lsp-ui-doc-mode
	"Minor mode for showing hover information in child frame."
	:init-value nil
	:group lsp-ui-doc
	(cond
	 (lsp-ui-doc-mode
      (with-eval-after-load 'frameset
		;; The documentation frame can’t be properly restored.  Especially
		;; ‘desktop-save’ will misbehave and save a bogus string "Unprintable
		;; entity" in the desktop file.  Therefore we have to prevent
		;; ‘frameset-save’ from saving the parameter.
		(unless (assq 'lsp-ui-doc-frame frameset-filter-alist)
          ;; Copy the variable first.  See the documentation of
          ;; ‘frameset-filter-alist’ for explanation.
          (cl-callf copy-tree frameset-filter-alist)
          (push '(lsp-ui-doc-frame . :never) frameset-filter-alist)))

      (add-hook 'lsp-on-hover-hook 'lsp-ui-doc--on-hover nil t)
      (add-hook 'delete-frame-functions 'lsp-ui-doc--on-delete nil t))
	 (t
      (remove-hook 'lsp-on-hover-hook 'lsp-ui-doc--on-hover t)
      (remove-hook 'delete-frame-functions 'lsp-ui-doc--on-delete t))))

  )
