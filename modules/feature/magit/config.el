;; modules/feature/magit/config.el    -*- lexical-binding: t; -*-

;; remove emacs version control VC interface
(setq vc-handled-backends nil)

;;; magit
(use-package magit

  :commands (magit-log-current
			 magit-diff-working-tree
			 magit-status
			 magit-status-prompt
			 fn-magit-status
			 fn-magit-log
			 counsel-git-checkout
			 magit-diff-visit-file-other)

  :bind (:map magit-mode-map
			  ("M-RET" . magit-diff-visit-file-other))

  :init
  (spacemacs/set-leader-keys
	"gl" 'magit-log-current
	"gd" 'magit-diff-working-tree
	"gs" 'magit-status
	"gC" '(lambda () (interactive) (counsel-git-checkout))
	"gf" '(lambda () (interactive) (magit-log-buffer-file))
	)

  :config
  (setq magit-refresh-status-buffer nil
		magit-branch-popup-show-variables nil)

  (eval-after-load "projectile"
	'(progn
	   (setq magit-repository-directories
			 (mapcar (lambda (dir)
					   (substring dir 0 -1))
					 (remove-if-not (lambda (project)
									  (file-directory-p (concat project "/.git/")))
									(projectile-relevant-known-projects))))
	   (setq magit-repo-dirs-depth 1)))

  (evil-define-key '(normal visual insert) magit-status-mode-map (kbd "C-t") 'tabbar-new-tab)
  (evil-define-key '(normal visual insert) magit-status-mode-map (kbd "C-w") 'fn-close-window)
  (evil-define-key '(normal visual insert) magit-mode-map (kbd "C-t") 'tabbar-new-tab)
  (evil-define-key '(normal visual insert) magit-mode-map (kbd "C-w") 'fn-close-window)
  (evil-define-key '(normal visual insert) magit-log-mode-map (kbd "C-t") 'tabbar-new-tab)
  (evil-define-key '(normal visual insert) magit-log-mode-map (kbd "C-w") 'fn-close-window)

  ;; use default bury function for log-buffers for tabbar compatibility
  (define-key magit-log-mode-map "q" #'magit-mode-bury-buffer)

  (evil-define-key 'normal magit-status-mode-map (kbd "C-b") 'pjson-ivy-compile-target)

  (evil-define-key 'emacs magit-popup-mode-map (kbd "<ESC>") 'magit-popup-quit)
  (evil-define-key 'emacs magit-popup-mode-map (kbd "/") 'evil-ex-search-forward)

  (evil-define-key '(visual normal evilified) magit-mode-map "gb" #'acc-ido-switch-buffer)

  ;; hl current-line
  (defface +magit-current-line-face
	`((t (:box (:line-width 1 :color ,(face-attribute 'highlight :background))
               :background ,(face-attribute 'highlight :background))))
	"Face used to highlight the current line in magit status and
	log buffer.")

  (defun +magit-hl-inverse-config ()
	(setq-local hl-line-face '+magit-current-line-face )
	(hl-line-mode 1))

  (add-hook 'magit-status-mode-hook #'+magit-hl-inverse-config)
  (add-hook 'magit-log-mode-hook #'+magit-hl-inverse-config)

  ;; do not show diffs when committing (-> show manually, on demand via C-c C-d)
  (remove-hook 'server-switch-hook 'magit-commit-diff)

  ;; -- for terminal magit interface
  (defun magit-status-prompt ()
	"Prompt for projectile project to show magit-status for."
	(interactive)
	(let ((current-prefix-arg '(4))) (call-interactively 'magit-status))
	)

  (defun fn-magit-status (args)
	"Open magit status in dir args[0]."
	(interactive)
	(magit-status (magit-toplevel (format "%s" (car args)))))

  (defun fn-magit-log ()
	"Open magit-log to head."
	;; TODO:  make optional to pass file(s)
	(interactive)
	(magit-log-head '("-n32")))

  ;; -- for viewing file at point in other window
  (defun magit-diff-visit-file-other ()
	"Open selected file in magit in other buffer. Create other buffer if it doesn't exist."
	(interactive)
	(when (eq (count-visible-buffers) 1)
	  (evil-window-vsplit))
	(let ((current-prefix-arg '(1))) (call-interactively 'magit-diff-visit-file)))

  ;; magit-status:  on files
  (define-key magit-file-section-map (kbd "M-RET") #'magit-diff-visit-file-other)

  ;; NOTE: used as magit-display-buffer-function
  (defun fn-magit-display-buffer-function (buffer)
	(interactive)
	;; TODO:  choose action (-> window.el::display-buffer) depending on available space
	;; TODO:  choose side to display wisely (below, left?)
	;; TODO:  if a revision-buffer is already open, reuse that
	;; TODO   New keys:
	;;   - tab: peek, opens new revision buffer but does not select
	;;   - C-n/C-p in log buffer:  select next/previous commit and show in rev buffer, but do not select
	;;   - C-n/C-p in rev buffer:  select next/previous commit and show, stay in rev buffer
	;;   - ctrl-enter in log buffer:    show commit in full-screen
	;;   - enter in log buffer:         show commit in rev buffer, jump there
	;; For all opening-rev-buffer actions:   do so only if it makes sense. If it doesnt, just use current buffer.
	;; (magit-display-buffer-traditional buffer))
	(display-buffer buffer '(display-buffer-same-window)) ;; NOTE: overrides purpose
	;; (display-buffer buffer)                             ;; NOTE: respects purpose
	)

  ;; (display-buffer
  ;;  buffer (if (and (derived-mode-p 'magit-mode)
  ;; 				   (memq (with-current-buffer buffer major-mode)
  ;; 						 '(magit-revision-mode)))
  ;; 			  '(display-buffer-pop-up-window)
  ;; 			'(display-buffer-same-window))))
  )

;;; magit-quick

(use-package feature/magit/+quick
  :commands (+magit-quick-commit
			 +magit-quick-add-untracked)

  :init
  (spacemacs/set-leader-keys "gc" '+magit-quick-commit)
  (spacemacs/set-leader-keys "gu" '+magit-quick-add-untracked))

;;; magit-todos

(use-package magit-todos
  :after magit
  :config
  (setq magit-todos-section-map '(keymap (13 . magit-todos-list)))
  )

;; (disable for now ,it 's not useable professionally without using dir-locals ,which
;; I can 't yet sensibly)
(run-with-timer 20 nil
				#'(lambda ()
					(setq magit-status-sections-hook
						  (remove #'magit-todos--insert-todos magit-status-sections-hook))))

(defun fn-fix-org-agenda ()
  ""
  (interactive)
  (require 'porg)
  (fn-config-org-agenda)
  (load "feature/org/config"))


;; -------------- window management ------------------------
;; NOTE: WIP?



