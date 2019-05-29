;; modules/ui/theme/config.el    -*- lexical-binding: t; -*-

;; -> theming
(setq x-underline-at-descent-line nil)

;; -> theming
(setq frame-background-mode 'dark)
;; ..is actually used somewhere
(defface hl-line-face
  '() "Bla" :group 'default)
;; performance fix
(run-with-idle-timer 5 nil '(lambda ()
							  (defalias 'mode-icons-set-minor-mode-icon (lambda () ))
							  (defun mode-icons-set-minor-mode-icon () )))

(global-vi-tilde-fringe-mode 1) 		; TODO: add exceptions, maybe have some list or something
(fringe-mode '(8 . 8))

;; -- hide mode line ---------------------
(spacemacs/toggle-mode-line-minor-modes-off)
;; general ui
;; nlinum format
(setq nlinum-format "%4d ")
;; Disable menubar
(menu-bar-mode -1)

;; -------- font:  spacing
;; TODO:  use set-face-attribute instead?
(setq default-text-properties '(line-spacing 0.05 line-height 1.05))
;; (setq default-text-properties '(line-spacing 0.05 line-height 1.06))

;; fringe indicator symbols
;; NOTE: meant for e.g. treemacs, doesnt work with buffer-local fringe-indicator-alists, apparently
(setf (cdr (assq 'continuation fringe-indicator-alist)) '(left-curly-arrow right-curly-arrow))
(setf (cdr (assq 'truncation fringe-indicator-alist)) '(nil nil))

;; -------- mouse
(set-mouse-color "green")

(defun set-mouse-color_ (new-frame)
  (set-mouse-color "green"))
(add-hook #'after-make-frame-functions #'set_mouse_color_)

;; -----------------------------------------------------------------------------
;;        treemacs
;; -----------------------------------------------------------------------------

(treemacs-resize-icons 14)

(defun +theme--treemacs-mode-hook ()
  (setq header-line-format nil))

(advice-add #'treemacs-mode :after #'+theme--treemacs-mode-hook)


;; -----------------------------------------------------------------------------
;;        modeline
;; -----------------------------------------------------------------------------

;; (feebleline-mode 1)

;; -> package minimal-modeline
(defun fn--hide-modeline-selectively ()
  (if nil
	  (setq mode-line-format nil)
	(progn
	  (defvar buffers-at-bottom '())
	  (defvar buffers-at-top '())
	  (walk-windows (lambda (window)
					  (with-current-buffer (window-buffer window)
						(if (/= (nth 1 (window-edges (minibuffer-window)))
								(nth 3 (window-edges window)))
							(add-to-list 'buffers-at-top (current-buffer))
						  (add-to-list 'buffers-at-bottom (current-buffer))
						  )))
					0)

	  (loop for buf in (cl-set-difference buffers-at-bottom buffers-at-top)
			do (with-current-buffer buf
				 (setq mode-line-format nil)))

	  (loop for buf in buffers-at-top
			do (with-current-buffer buf
				 (setq mode-line-format " ")
				 )))

	(makunbound 'buffers-at-bottom)
	(makunbound 'buffers-at-top))
  )

(add-hook 'window-configuration-change-hook #'fn--hide-modeline-selectively)

;; mode icons (disabled in buffers)
(require 'mode-icons)
(setq mode-icons-change-mode-name nil)
(remove-hook 'helm-mode-hook 'mode-icons-mode)
(remove-hook 'helm-minibuffer-set-up-hook 'mode-icons-mode)
(mode-icons-mode)
;; fancy git icon
(defadvice vc-mode-line (after strip-backend () activate)
  (when (stringp vc-mode)
    (let ((gitlogo (replace-regexp-in-string "^ Git." "  " vc-mode)))
      (setq vc-mode gitlogo))))

(defun customize-face-fn ()
  "Like customize-face, but ignores current line highlight face."
  (interactive)
  (spacemacs/toggle-highlight-current-line-globally-off)
  (call-interactively 'customize-face)
  (spacemacs/toggle-highlight-current-line-globally-on)
  )

(set-face-attribute 'trailing-whitespace nil :background "grey17")

;; vertical separator line
(defun my-change-window-divider ()
  (let ((display-table (or buffer-display-table standard-display-table)))
    (set-display-table-slot display-table 5 ?│)
    (set-window-display-table (selected-window) display-table)))

;; TODO: wtf, how often is this called then?
(add-hook 'window-configuration-change-hook 'my-change-window-divider)


