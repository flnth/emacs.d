;; -*- lexical-binding: t -*-

;; -- convert &gt symbols in redmine mails -----------
(defun fn--html2text-if-gt-symbols ()
  (save-excursion
	(goto-char (point-min))
	(when (search-forward-regexp "^&gt" nil t)
	  (html2text))))
(advice-add 'mu4e~fontify-cited :before #'fn--html2text-if-gt-symbols)

;; (advice-remove 'message-mode-hook #'fn--html2text-if-gt-symbols)
;; (advice-add ' :after #'fn--html2text-if-gt-symbols)

;; -- prettify citing
(defun fn--mu4e-article-fill-cited (msg)
  (interactive)
  (save-mark-and-excursion
	;; (gnus-article-fill-cited-article fill-column)
	)
  ;; (run-with-timer 0.1 nil #'(lambda () (interactive)
  ;; 							  (goto-char (point-min))))
  )

;; -- citations in mu4e-compose-mode (overwritten)
(define-minor-mode gnus-message-citation-mode
  "Minor mode providing more font-lock support for nested citations.
When enabled, it automatically turns on `font-lock-mode'."
  nil ;; init-value
  "" ;; lighter
  nil ;; keymap
  (when (eq major-mode 'mu4e-compose-mode)   ;NOTE:  EDITED
    ;; FIXME: Use font-lock-add-keywords!
    (let ((defaults (car font-lock-defaults))
	  default keywords)
      (while defaults
	(setq default (if (consp defaults)
			  (pop defaults)
			(prog1
			    defaults
			  (setq defaults nil))))
	(if gnus-message-citation-mode
	    ;; `gnus-message-citation-keywords' should be the last
	    ;; elements of the keywords because the others are unlikely
	    ;; to have the OVERRIDE flags -- XEmacs applies a keyword
	    ;; having no OVERRIDE flag to matched text even if it has
	    ;; already other faces, while Emacs doesn't.
	    (set (make-local-variable default)
		 (append (default-value default)
			 gnus-message-citation-keywords))
	  (kill-local-variable default))))
    ;; Force `font-lock-set-defaults' to update `font-lock-keywords'.
    (setq font-lock-set-defaults nil)
    (font-lock-set-defaults)
    (if font-lock-mode
	(font-lock-flush)
      (gnus-message-citation-mode (font-lock-mode 1)))))

;; TODO: ...which one now?
(advice-add 'mu4e~view-internal :after #'fn--mu4e-article-fill-cited)
(advice-add 'mu4e~view-internal :after #'gnus-article-fill-cited-article)
