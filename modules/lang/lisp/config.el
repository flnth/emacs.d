;; modules/lang/lisp/config.el    -*- lexical-binding: t; -*-

(with-eval-after-load 'slime
  (evil-define-key 'normal slime-repl-mode-map (kbd "C-n") #'slime-repl-next-input)
  (evil-define-key 'normal slime-repl-mode-map (kbd "C-p") #'slime-repl-previous-input)

  (evil-define-key 'insert slime-repl-mode-map (kbd "C-n") #'slime-repl-next-input)
  (evil-define-key 'insert slime-repl-mode-map (kbd "C-p") #'slime-repl-previous-input))
(with-eval-after-load 'lispy
  (evil-define-key '(normal visual insert) lispy-mode-map (kbd "C-t") 'tabbar-new-tab)
  (evil-define-key '(normal visual insert) lispy-mode-map (kbd "C-w") 'fn-close-window)
  )

;;;; lisp

(add-to-list 'load-path (concat dir_emacs "packages/lispy"))
(add-to-list 'load-path (concat dir_emacs "packages/lispyville-mod"))

(require 'lispy)
(require 'lispyville)

(defun +lisp-open-above ()
  "Like evil-open-above, but does not leave whitespace behind."
  (interactive)
  (delete-horizontal-space)
  (evil-open-above 1))

(defun +lisp-open-below ()
  "Like evil-open-below, but does not leave whitespace behind."
  (interactive)
  (delete-horizontal-space)
  (evil-open-below 1))

(with-eval-after-load 'lispyville
  (require 'smartparens)
  (add-hook 'lispy-mode-hook #'lispyville-mode)
  (lispyville-set-key-theme
   '(lispy
	 c-digits
	 special
	 text-objects
	 atom-ovement
	 atom-motions
	 additional-movement
	 additional-motions
	 slurp/barf-cp
	 operators
	 c-w
	 ))

  ;; keys:
  (define-key lispy-mode-map-lispy "[" nil)
  (define-key lispy-mode-map-lispy "]" nil)

  (evil-define-key 'normal lispyville-mode-map
	(kbd "C-k") #'spacemacs/evil-smart-doc-lookup
	(kbd "J") (lambda () (interactive)
				(lispyville-next-opening)
				(evil-forward-char))
	(kbd "L") #'lispyville-forward-sexp
	(kbd "K") #'sp-backward-up-sexp
	(kbd "O") #'+lisp-open-above
	(kbd "o") #'+lisp-open-below
	(kbd "{") #'evil-backward-paragraph
	(kbd "}") #'evil-forward-paragraph
	(kbd "<backtab>") #'outshine-cycle-buffer
	(kbd "C-M-<tab>") #'fn-hs-toggle-all)

  (evil-define-key 'visual lispyville-mode-map
	(kbd "{") #'evil-backward-paragraph
	(kbd "}") #'evil-forward-paragraph)

  (evil-define-key 'insert lispyville-mode-map
	(kbd "C-k") #'+lisp-open-above)
  )

;;;;; outlines
;; insert/promote/demote

;; -> module lisp

(defun fn--lispyville->-handler ()
  (interactive)
  (if (outline-on-heading-p)
	  (outline-demote)
	(call-interactively 'lispyville->)))

(defun fn--lispyville-<-handler ()
  (interactive)
  (if (outline-on-heading-p)
	  (outline-promote)
	(call-interactively 'lispyville-<)))

(evil-define-key '(normal visual) lispyville-mode-map
  (kbd ">") 'fn--lispyville->-handler
  (kbd "<") 'fn--lispyville-<-handler)

(evil-define-key '(normal visual insert) lispyville-mode-map
  (kbd "M-RET") 'outshine-insert-heading)

;;;;; outline movement
;; next/previous

;; -> to other outline stuff / code-navigation, etc.

(evil-define-key '(normal visual insert) lispyville-mode-map
  (kbd "M-n") 'outline-next-heading
  (kbd "M-p") 'outline-previous-heading)

;;;;; atom movement:   strings/comments
;; make e/b move by words inside strings and comments

;; -> lisp module

(require 'evil-nerd-commenter)

(defun fn--lispyville-e-handler ()
  (interactive)
  (if (or (in-string-p)
		  (evilnc--in-comment-p (point))
		  )
	  (evil-forward-word-end)
	(lispyville-forward-atom-begin)))

(defun fn--lispyville-b-handler ()
  (interactive)
  (if (or (in-string-p)
		  (evilnc--in-comment-p (point)))
	  (evil-backward-word-begin)
	(lispyville-backward-atom-begin)))

(evil-define-key '(normal visual) lispyville-mode-map
  (kbd "e") 'fn--lispyville-e-handler
  (kbd "b") 'fn--lispyville-b-handler)

;;;;; open above/below

(defun fn--lispyville-open-above-handler ()
  (interactive)
  (if (or (lispyville--top-level-p)
		  (in-string-p))
	  (evil-open-above 1)
	(lispyville-open-above-list 1)))

(defun fn--lispyville-open-below-handler ()
  (interactive)
  (if (or (lispyville--top-level-p)
		  (in-string-p))
	  (evil-open-below 1)
	(lispyville-open-below-list 1)))

(evil-define-key '(normal visual) lispyville-mode-map
  ;; (kbd "o") 'fn--lispyville-open-below-handler
  ;; (kbd "O") 'fn--lispyville-open-above-handler
  (kbd "o") 'evil-open-below
  (kbd "O") 'evil-open-above
  )

;;;;; defun-wise navigation

;; (evil-define-key '(normal visual insert) emacs-lisp-mode-map
;;   (kbd "C-n") nil
;;   (kbd "C-p") nil)

(evil-define-key '(normal visual) lispyville-mode-map
  (kbd "C-n") '(lambda () (interactive)  (beginning-of-defun -1))
  (kbd "C-p") '(lambda () (interactive) (beginning-of-defun 1)))

;;;; config

;; -> lisp

(defun my-lisp-mode-config ()
  "For use in `lisp-modes'."
  ;;(spacemacs/toggle-automatic-symbol-highlight-on)
  (define-key emacs-lisp-mode-map (kbd "<C-tab>") 'hs-toggle-hiding)
  (define-key emacs-lisp-mode-map (kbd "C-e") 'elisp-slime-nav-find-elisp-thing-at-point)
  (define-key emacs-lisp-mode-map (kbd "S-e") 'spacemacs/nav-find-elisp-thing-at-point-other-window)
  (define-key emacs-lisp-mode-map (kbd "<f4>") '+utils-modules-toggle-config) ; TODO: dependent on load-reihenfolge (!)
  (define-key emacs-lisp-mode-map (kbd "M-O s") '+utils-modules-toggle-config) ; TODO: dependent on load-reihenfolge (!)
  (set (make-local-variable 'outshine-regexp-base-char) ".")
  (set (make-local-variable 'outshine-outline-regexp-base) ";;")
  (set (make-local-variable 'outline-regexp) ";;[;]\{1,8\} ")
  (filladapt-mode 1)
  (outline-minor-mode 1)
  (rainbow-delimiters-mode -1)
  (lispy-mode))

;; emacs-lisp:   ,sr to send region to ielm
(defun ielm/send-region ()
  (interactive)
  (let ((text (buffer-substring-no-properties (region-beginning)
											  (region-end))))
	(with-current-buffer "*ielm*"
	  (insert text)
	  (ielm-send-input))))

(require 'ert)
(spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode (kbd "tt") #'ert )
(spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode (kbd "tr") #'+lisp-run-last-ert-test )
(spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode (kbd "sr") 'ielm/send-region )
(spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode (kbd "eb") (lambda ()
																		(interactive)
																		(save-mark-and-excursion
																		  (eval-buffer))) )

(add-hook 'lisp-mode-hook 'my-lisp-mode-config t)
(add-hook 'elisp-slime-nav-mode-hook 'my-lisp-mode-config)
(run-with-timer 2 nil #'(lambda ()
						  (add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-config t)))

(defun +lisp-inferior-emacs-lisp-mode-hook ()
  (setq ielm-header "")
  (setq header-line-format nil)
  (setf lexical-binding t)
  (vi-tilde-fringe-mode -1)

  (define-key ielm-map (kbd "C-n") #'comint-next-input)
  (define-key ielm-map (kbd "C-p") #'comint-previous-input)
  (define-key ielm-map (kbd "C-j") nil)
  (evil-define-key '(normal insert) comint-mode-map (kbd "C-j") nil)
  (define-key ielm-map (kbd "C-j") #'newline-and-indent)
  (define-key ielm-map (kbd "C-l") #'comint-clear-buffer)
  (evil-define-key '(normal) ielm-map (kbd "=")
	#'(lambda () (interactive)
		(let ((inhibit-message t))
		  (lisp-indent-region (region-beginning) (region-end)))))
  (define-key ielm-map (kbd "{") #'comint-previous-prompt)
  (define-key ielm-map (kbd "}") #'comint-next-prompt)
  (define-key ielm-map (kbd "<RET>") #'(lambda () (interactive)
										 (let ((inhibit-message t))
										   (goto-char (point-max))
										   (ielm-return))))

  ;; TODO: make that which has been input traversably-selectable using these
  ;; functions
  (evil-define-key '(visual) ielm-map (kbd "{")
	#'(lambda () (interactive)
		(let ((beg (region-beginning))
			  (end (region-end))
			  prompt-target)
		  (save-mark-and-excursion
			(deactivate-mark)
			(comint-previous-prompt 1)
			(setf prompt-target (point)))
		  (set-mark prompt-target)
		  (exchange-point-and-mark)
		  )))

  (evil-define-key '(visual) ielm-map (kbd "}")
	#'(lambda () (interactive)
		(let ((beg (region-beginning))
			  (end (region-end))
			  prompt-target
			  prompt-source)
		  (save-mark-and-excursion
			(deactivate-mark)
			(comint-next-prompt 1)
			(setf prompt-target (point)))
		  (exchange-point-and-mark)
		  (set-mark prompt-target)
		  (exchange-point-and-mark)
		  )))

  )

;; (add-hook 'inferior-emacs-lisp-mode-hook #'+lisp-inferior-emacs-lisp-mode-hook)
(add-hook 'inferior-emacs-lisp-mode-hook #'+lisp-inferior-emacs-lisp-mode-hook)
(add-hook 'ielm-mode-hook #'+lisp-inferior-emacs-lisp-mode-hook)

;; Force reloading of ielm when it has been opened for the first time.
;; Is not configured correctly on first-time load (whyever that is...)
(setq lisp--ielm-reloaded nil)
(defun lisp--ensure-ielm-reloaded (f &rest args)
  (funcall f args)
  (run-with-timer 1 nil
				  (lambda ()
					(when (not lisp--ielm-reloaded)
					  (if (string= (buffer-name) "*ielm*")
						  (kill-buffer-and-window)
						(kill-buffer "*ielm*"))
					  (funcall f args)
					  (setq lisp--ielm-reloaded t)
					  ))))

(advice-add #'ielm :around #'lisp--ensure-ielm-reloaded)



;; -----------------------------------------------------------------------------
;;        racket
;; -----------------------------------------------------------------------------

;; -> lisp mode

(evil-define-key 'normal racket-mode-map
  (kbd ",,") 'evil-lisp-state)


;;;; font-lock

(defun +lisp-customize-font-lock ()
  (interactive)

  ;; highlight .symbols in let-alist :)
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("\\_<\\.\\(?:\\sw\\|\\s_\\)+\\_>" 0
	  font-lock-builtin-face)))

  (defface lisp-variable-name-face
	'((t ()))
	"Face for coloring variable names using lisp-extra-font-lock.")

  (defface lisp-warning-face
	'((t ()))
	"Face for coloring variable names using lisp-extra-font-lock.")

  (defface lisp-function-face
	'((t ()))
	"Face for coloring function names in lisp-modes.")

  ;; remove hooks if present
  (remove-hook 'emacs-lisp-mode-hook #'+lisp-font-lock-redefine)
  (remove-hook 'emacs-lisp-mode-hook 'lisp-extra-font-lock-mode)
  (remove-hook 'emacs-lisp-mode-hook 'hdefd-highlight-mode)
  (remove-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode)

  (defun +lisp-font-lock-redefine ()
	(setq-local font-lock-variable-name-face
				'lisp-variable-name-face)
	(setq-local font-lock-warning-face
				'lisp-warning-face)
	(setq-local font-lock-function-name-face
				'lisp-function-face)
	)

  (add-hook 'emacs-lisp-mode-hook #'+lisp-font-lock-redefine)

  ;; highlight defined mode
  ;; (add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode)

  ;; NOTE: the order in which these are added, matters
  ;; lisp-extra-font-lock mode
  (require 'lang/lisp/hl-defined)
  (add-hook 'emacs-lisp-mode-hook 'hdefd-highlight-mode)

  ;; highlight quoted
  (require 'highlight-quoted)
  (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode)

  ;; lisp-extra-font-lock mode
  (require 'lang/lisp/lisp-extra-font-lock)
  (add-hook 'emacs-lisp-mode-hook 'lisp-extra-font-lock-mode))

(run-with-timer 20 nil #'+lisp-customize-font-lock)

;;;; comments, doc-strings
;; using filladapt-mode

(defun +lisp-newline-and-indent ()
  ""
  (interactive)
  (if (eq (get-char-property (point) 'face)
		  'font-lock-doc-face)
	  ;; inside doc-string
	  (let* ((last-col-point (save-excursion
							   (end-of-line)
							   (point)))
			 (beg-line-point (save-excursion
							   (beginning-of-line)
							   (point)))
			 (first-col (save-excursion
						  (beginning-of-line)
						  (re-search-forward "[^\s\\|^\t\\|^\"]" last-col-point t)
						  (current-column))))

		;; do not leave whitespace behind
		(save-excursion
		  (goto-char last-col-point)
		  ;; (move-to-column (+ (current-column) 1) t)
		  (re-search-backward "^\\|[^\s\\|^\t]" beg-line-point t)
		  ;; (delete-region (point) (+ 1 (point)))
		  (delete-region (if (or (looking-at "[[:space:]]")
								 (eq beg-line-point last-col-point))
		  					 (point)
		  				   (+ 1 (point)))
		  				 last-col-point))
		(newline)

		;; style:  do not indent last "
		(when (not (eq (char-after (point)) 34))
		  (insert (make-string (max 0 (- first-col 1)) ? )))
		)

	;; outside doc-string
	(lispy-newline-and-indent-plain)
	)
  )

(defun +lisp-tab ()
  ""
  (interactive)

  ;; inside docstring?
  (if (eq (get-char-property (point) 'face)
		  'font-lock-doc-face)
	  (let* ((cur-point (point))
			 (col-max-point (save-excursion
							  (end-of-line)
							  (point)))
			 (col-min-point (save-excursion
							  (beginning-of-line)
							  (point)))
			 (whitespace-before (save-excursion
								  (and (null (re-search-backward "[^\s\\|^\t]" col-min-point t))
									   (not (eq cur-point col-min-point)))
								  ))
			 (whitespace-ahead (save-excursion
								 (and (null (re-search-forward "[^\s\\|^\t]" col-max-point t))
									  (not (eq cur-point col-max-point))
									  ))))

		(cond
		 (whitespace-ahead 				; -> goto to its end
		  (goto-char col-max-point))

		 ((or whitespace-before
			  (eq col-min-point col-max-point)) ; indent according to line above
		  (kill-whole-line)
		  (goto-char (- (point) 1))
		  (+lisp-newline-and-indent))

		 (t								; default:  -> lispy-tab
		  (message "lispy-tab")
		  (lispy-tab))))

	;; usual lisp mode, just use lispy-tab
	(lispy-indent-adjust-parens 0)
	))

(with-eval-after-load 'lispy
  (evil-define-key '(insert) lispy-mode-map
	(kbd "RET") #'+lisp-newline-and-indent
	(kbd "TAB") #'+lisp-tab)
  )

;;;; trailing whitespace cleanup
(defun +utils-del-trailing-whitespace-around-wrap ()
  (interactive)
  (let ((cur (current-column)))
	(cond
	 ((and (eq cur 0)
		   (save-excursion
			 (end-of-line)
			 (eq cur (current-column)))
		   )
	  (save-excursion
		(evil-previous-visual-line)
		(end-of-line)
		(delete-horizontal-space t)
		(evil-next-visual-line)
		(end-of-line)
		(delete-horizontal-space t)
		(evil-next-visual-line)
		(end-of-line)
		(delete-horizontal-space t)))
	 (t
	  (funcall-interactively #'lispyville-delete-char-or-splice (point) (+ (point) 1)))
	 )))

(with-eval-after-load 'lispy
  (evil-define-key '(normal) lispy-mode-map
	(kbd "x") #'+utils-del-trailing-whitespace-around-wrap
	)
)

;;;; dynamic macro expansion
(use-package quick-peek
  :load-path "packages/quick-peek")
(require 'lang/lisp/hexpand)
(with-eval-after-load 'lispy
  (evil-define-key '(normal) lispy-mode-map
	(kbd "C-M-z") #'hexpand-quick-peek-expand-macro))


;; after ui/evil
(evil-define-key 'visual evil-surround-mode-map (kbd "s") #'+evil-surround-region)


