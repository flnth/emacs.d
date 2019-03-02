;; modules/ui/general/config.el    -*- lexical-binding: t; -*-

(setq dotspacemacs-highlight-delimiters nil)
(global-highlight-parentheses-mode -1)
(spacemacs/toggle-smartparens-globally-on)
(show-smartparens-global-mode -1)
;; -- company mode, completion cl-struct-ert-test-aborted-with-non-local-exit-tags
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-u") #'(lambda () (interactive) (company-select-previous 4)))
  (define-key company-active-map (kbd "C-d") #'(lambda () (interactive) (company-select-next 4)))
  (global-set-key (kbd "\M- ") 'company-complete)
  (define-key global-map (kbd "C-SPC") #'company-complete)
  (define-key global-map (kbd "C-@") #'company-complete))
(defun rainbow-colorize-match (color &optional match)
  "Return a matched string propertized with a face whose
background is COLOR. The foreground is computed using
`rainbow-color-luminance', and is either white or black."
  (let ((match (or match 0)))
    (put-text-property
     (match-beginning match) (match-end match)
     ;; 'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
     ;;                           "white" "black"))
     ;;         (:background ,color)))))
     'face `((:foreground ,color)
             (:background ,nil)))))
(require 'zeal-at-point)
(add-to-list 'zeal-at-point-mode-alist '(python-mode . "python,numpy,scipy,matplotlib"))
(add-to-list 'zeal-at-point-mode-alist '(c++-mode . ""))
(add-to-list 'zeal-at-point-mode-alist '(c-mode . ""))
(add-to-list 'zeal-at-point-mode-alist '(racket-mode . "racket"))

;; TODO: change key, actually
(global-set-key (kbd "<f1>") 'zeal-at-point)
(require 'yascroll)
(require 'yasnippet)

(global-yascroll-bar-mode)
(yas-reload-all)
;;;; textile mode (general ui)

(require 'textile-mode)
;;;; dired (general ui or dired module)

(use-package help-fns+
  :load-path "packages/help-fns-plus")
(require 'dired+)
(diredp-make-find-file-keys-reuse-dirs)

(defun fn-dired (&optional dir term)
  (interactive)
  ;; (if (null dir)
  ;; 	  (dired default-directory)
  ;; 	(dired dir))

  ;; ;; change q to close frame if we came from a terminal
  ;; (when (not (null term))
  ;; 	;; change key

  ;; 	;; install callback that removes the callback after

  ;; 	)
  )

;; plantuml
(setq plantuml-jar-path (concat dir_system "/plantuml.jar"))
;; C-x C-c to kill the frame and not exit the client (and prompt)...
;; (global-set-key (kbd "C-x C-c") 'delete-frame)
(global-set-key (kbd "C-x C-c") 'nil)
(global-set-key (kbd "C-x C-s") 'nil)
;; font size

;; -> general ui

(setq frame-font-increment 10)
(global-set-key (kbd "C-=") 'frame-font-bigger)
(global-set-key (kbd "C--") 'frame-font-smaller)
;; (global-set-key (kbd "C-=") 'spacemacs/zoom-frm-in)
;; (global-set-key (kbd "C--") 'spacemacs/zoom-frm-out)
;; (global-set-key (kbd "<C-mouse-4>") 'spacemacs/zoom-frm-in)
;; (global-set-key (kbd "<C-mouse-5>") 'spacemacs/zoom-frm-out)


;; mouse
(global-set-key (kbd "<mouse-4>") (lambda () 'scroll-down-line 4))
(global-set-key (kbd "<mouse-5>") (lambda () 'scroll-up-line 4))

;; copy-paste in X gui window
(global-set-key (kbd "C-S-v") 'clipboard-yank )
;; -- completion / minibuffer -----------------------------

;; disable helm
(helm-mode -1)

;; restore defaults
(setq completing-read-function 'completing-read-default)
;; (setq completing-read-function 'ivy-completing-read)
(setq completion-in-region-function #'completion--in-region)
;; (setq completion-in-region-function #'ivy-completion-in-region)

(setq completion-cycle-threshold t)

(defun fn-ivy-minibuffer-complete ()
  (interactive)
  (ivy-read "> "
			minibuffer-completion-table
			:initial-input (minibuffer-contents)
			:sort nil
			:action '(lambda (selection) (interactive)
					   (let ((inhibit-read-only t)
							 (selection (if (listp selection)
											(car selection)
										  selection)))
						 (setf (buffer-string) (substring-no-properties selection))
						 (minibuffer-complete-and-exit)))
			:caller #'fn-ivy-minibuffer-complete))
(define-key minibuffer-local-map (kbd "<C-tab>") #'fn-ivy-minibuffer-complete )
(evil-define-key '(normal) minibuffer-local-map (kbd "dd")
  #'(lambda () (interactive) (delete-region (line-beginning-position) (line-end-position))))

;; M-DEL to delete rather than kill, includes forward slashes for path navigation
(evil-define-key '(normal insert) minibuffer-local-map (kbd "<M-DEL>")
  (lambda () (interactive)
	(save-excursion
	  (goto-char (- (point) 1))
	  (when (looking-at "\/")
		(delete-char 1)))
	(evil-delete-backward-word)))

(define-key minibuffer-local-completion-map (kbd "<SPC>") '(lambda () (interactive) (insert " ")) )

;; helpful:  uses completing-read under the hood. --> wrap all calls.
(defmacro with-ivy-completing-read (body)
  `(let ((completing-read-function #'ivy-completing-read))
	 (,@body)))
;; help

(defun +helpful-function-macro (symbol)
  "Show help for function named SYMBOL."
  (interactive
   (list (helpful--read-symbol "Function/Macro: "
							   #'(lambda (x) (or (functionp x) (macrop x))))))
  (funcall helpful-switch-buffer-function (helpful--buffer symbol t))
  (helpful-update))

(defun fn-helpful-function ()
  (interactive)
  (with-ivy-completing-read
   (call-interactively #'+helpful-function-macro)
   ))

(defun fn-helpful-variable ()
  (interactive)
  (with-ivy-completing-read
   (call-interactively #'helpful-variable)))

(run-with-idle-timer 5 nil '(lambda () (interactive)
							  (spacemacs/set-leader-keys "hdf" 'fn-helpful-function)
							  (spacemacs/set-leader-keys "hdv" 'fn-helpful-variable)
							  (spacemacs/set-leader-keys "hdk" 'helpful-key)))
(define-key evil-motion-state-map (kbd "C-S-s") #'projectile-save-project-buffers)
;; -> access/avy

(spacemacs/set-leader-keys
  "jj" 'avy-goto-char-timer
  "jJ" 'avy-goto-char)
(spacemacs/set-leader-keys "ad" '(lambda () (interactive) (dired default-directory)))


(defun +ui-bury-kill-buffer (&optional buffer-or-name)
  "Bury the current buffer (to restore what has been shown
before), then make sure it is killed. Useful e.g. for 'q' in
dired."
  (interactive)
  (let ((current-buffer (current-buffer)))
	(bury-buffer)
	(kill-buffer current-buffer)))

(evil-define-key '(normal visual) dired-mode-map
  "h" #'diredp-up-directory-reuse-dir-buffer
  "l" #'diredp-find-file-reuse-dir-buffer
  "q" #'+ui-bury-kill-buffer
  ;; TODO: depend on window-navigation module
  (kbd "M-h") '(lambda () (interactive) (fn-navigate 'left))
  (kbd "M-j") '(lambda () (interactive) (fn-navigate 'below))
  (kbd "M-k") '(lambda () (interactive) (fn-navigate 'above))
  (kbd "M-l") '(lambda () (interactive) (fn-navigate 'right))
  ;; TODO: depend on evil module
  "n" #'(lambda () (interactive) (fn-evil-ex-search 'evil-ex-search-next))
  "N" #'(lambda () (interactive) (fn-evil-ex-search 'evil-ex-search-previous))
  )
;;;; indentation

;; -> module editing tools (or something)
;; (together with the comment line feature, somehow!)

(defun fn-indent ()
  (interactive)
  (if (use-region-p)
	  (let ((beg (region-beginning))
			(end (region-end)))
		(funcall indent-region-function beg end)
		(goto-char beg)
		(indent-region beg end)
		;; We also need to tabify or untabify the leading white characters
		(when evil-indent-convert-tabs
		  (let* ((beg-line (line-number-at-pos beg))
				 (end-line (line-number-at-pos end))
				 (ln beg-line)
				 (convert-white (if indent-tabs-mode 'tabify 'untabify)))
			(save-excursion
			  (while (<= ln end-line)
				(goto-char (point-min))
				(forward-line (- ln 1))
				(back-to-indentation)
				;; Whether tab or space should be used is determined by indent-tabs-mode
				(funcall convert-white (line-beginning-position) (point))
				(setq ln (1+ ln)))))
		  (back-to-indentation)))
	(evil-indent (line-beginning-position) (line-beginning-position 2))))

(define-key evil-normal-state-map (kbd "=") 'fn-indent )

;; comment indentation
;; -> evil
(global-set-key (kbd "M-;") (lambda () (interactive) (comment-dwim nil) (evil-insert nil)  ))
;; -> general ui
(global-undo-tree-mode -1)
(use-package redo+
  :load-path "packages/redo-plus")

;; -> general ui
(global-anzu-mode -1)
;; do not ask before killing a buffer that has a running process
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))
;; firefox as default browser
(setq browse-url-browser-function 'browse-url-firefox)
;; make gui frame ----------
(defun fn-make-frame-scratchpad ()
  (make-frame '((window-system . x)
				(name . "scratchemacs"))))
;; -> general ui
(defun fn-paste-in-minibuffer ()
  (local-set-key (kbd "C-S-v") 'paste-from-x-clipboard)
  )
;; for full-document previews using latex
(add-hook 'doc-view-mode-hook 'auto-revert-hook)
;; ...maybe... put on timer to execute after spacemacs shit
(setq recenter-redisplay nil)

(eval-after-load "custom"
  '(evil-define-key 'normal custom-mode-map "q" 'Custom-buffer-done))

;;;; indentation, tabs, spaces
;; (defun infer-indentation-style ()
;;   ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
;;   ;; neither, we use the current indent-tabs-mode
;;   (let ((space-count (how-many "^  " (point-min) (point-max)))
;; 		(tab-count (how-many "^\t" (point-min) (point-max))))
;; 	(if (> space-count tab-count) (setq indent-tabs-mode nil))
;; 	(if (> tab-count space-count) (setq indent-tabs-mode t))))

(global-set-key (kbd "<f4>") 'projectile-find-other-file)
(global-set-key (kbd "M-O s") 'projectile-find-other-file)
;; -- clang tools
;; Bind clang-format-region to C-M-tab in all modes:
;; (global-set-key [C-M-tab] 'clang-format-region)

;; Bind clang-format-buffer to tab on the c++-mode only:
;; (add-hook 'c++-mode-hook 'clang-format-bindings)
;; (defun clang-format-bindings ()
;;   (define-key c++-mode-map [tab] 'clang-format-buffer))
;; f11 to toggle fullscreen
(global-set-key (kbd "<f11>") 'spacemacs/toggle-fullscreen)

;;;; customize interface

;; TODO: either put in module access, or provide its own module...
;; TODO: custom-goto-parent and go-back for forward/previous navigation?
;;       maybe even do implement a little history or something...

(defun +customize--next-x (f reg char &optional line end-or-beginning)
  "Returns line number on which next x specced by (REG,CHAR) is
using F which should either be `re-search-forward` or
`re-search-backward`, starting at LINE. Returns nil if there's no
next x."
  (save-excursion
	(when line
	  (goto-char (point-min))
	  (forward-line (1- line))
	  (if end-or-beginning
		  (end-of-line)
		(beginning-of-line)))
	(if (funcall f reg nil t)
		(progn
		  (beginning-of-line)
		  (line-number-at-pos))
	  nil)))

(defun +customize-next-entry ()
  (interactive)
  (let* ((start (+ (line-number-at-pos) 1))
		 (next-closed (+customize--next-x #'re-search-forward "^Show" "S" start nil))
		 (next-open (+customize--next-x #'re-search-forward "^Hide" "H" start nil))
		 (line-candidate (or (ignore-errors (min next-closed next-open)) next-closed next-open)))
	(when line-candidate
	  (goto-char (point-min))
	  (forward-line (1- line-candidate)))))

(defun +customize-prev-entry ()
  (interactive)
  (let* ((start (- (line-number-at-pos) 1))
		 (prev-closed (+customize--next-x #'re-search-backward "^Show" "S" start t))
		 (prev-open (+customize--next-x #'re-search-backward "^Hide" "H" start t))
		 (line-candidate (or (ignore-errors (max prev-closed prev-open)) prev-closed prev-open)))
	(when line-candidate
	  (goto-char (point-min))
	  (forward-line (1- line-candidate)))))

(defun +customize-apropos-search ()
  "Performs a customize apropos search in the current window,
asking for the string to query in the minibuffer."
  (interactive)
  (let* ((search (read-from-minibuffer "customize-apropos> ")))
	(when search
	  (customize-apropos (split-string search)))))

(defun +customize-group-search ()
  "Performs a customize apropos search in the current window,
asking for the string to query in the minibuffer."
  (interactive)
  (let* ((search (read-from-minibuffer "customize-group> ")))
	;; TODO: optionally use counsel for selection
	(when search
	  (customize-group search))))

(evil-define-key 'normal Custom-mode-map
  (kbd "C-n") #'+customize-next-entry
  (kbd "M-n") #'+customize-next-entry
  (kbd "C-p") #'+customize-prev-entry
  (kbd "M-p") #'+customize-prev-entry
  (kbd "<C-tab>") #'Custom-newline
  (kbd "M-<tab>") #'Custom-newline
  (kbd "C-s")  #'+customize-apropos-search)

(spacemacs/set-leader-keys (kbd "cs") #'+customize-apropos-search)
(spacemacs/set-leader-keys (kbd "cg") #'+customize-group-search)

;;;; man interface
;; TODO: put somewhere else, cleanup

(require 'evil-collection)
(require 'evil-collection-man)
(evil-collection-man-setup)

(evil-define-key '(normal visual evilified insert motion) Man-mode-map
  (kbd "C-w") #'fn-close-window
  (kbd "C-n") #'Man-next-section
  (kbd "C-p") #'Man-previous-section
  "gr" #'Man-update-manpage
  "gm" #'Man-goto-section
  )

;; TODO: man toggle...?

;;;; info interface
(require 'evil-collection-info)
(evil-collection-info-setup)

(evil-define-key '(normal visual evilified insert motion) Info-mode-map
  (kbd "M-h") #'(lambda () (interactive) (fn-navigate 'left))
  (kbd "C-S-l") #'Info-next
  (kbd "C-S-h") #'Info-prev
  (kbd "C-S-k") #'Info-up
  (kbd "C-S-j") #'Info-history-back
  (kbd "C-,") #'Info-history-back
  (kbd "C-.") #'Info-history-forward
  "gb" #'acc-ido-switch-buffer
  "G" #'evil-goto-line
  "gg" #'evil-goto-first-line
  (kbd "TAB") #'Info-next-reference
  (kbd "<mouse-4>") #'scroll-up-half
  (kbd "<mouse-5>") #'scroll-down-half)

(spacemacs/set-leader-keys "gi" #'info)

;; TODO: info toggle...?

;;;; dired





