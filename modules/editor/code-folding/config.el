;; modules/editor/code-folding/config.el    -*- lexical-binding: t; -*-

;;; outlining
;; org-mode hierarchies and folding in code files

(require 'dash)

(use-package outline
  :commands (outline-minor-mode toggle-hiding-outline-all)
  :hook (prog-mode . outline-minor-mode) ; outline-minor-mode on prog-mode-hook
  :bind (("<C-iso-lefttab>" . 'toggle-hiding-outline-all)
		 ("M-n" . 'outline-next-heading)
		 ("M-p" . 'outline-previous-heading))
  :init
  (defvar outline-minor-mode-prefix "M-#")
  :config

  (defun toggle-hiding-outline-all ()
	(interactive)
	;; is local variable set?  no -> unhide all, set the variable
	;; yes -> read its state.
	;; if hidden == t, unhide all
	;; if hidden == nil, hide all
	(if (local-variable-p 'outline-hidden)
		(if (equal outline-hidden t)
			(progn
			  (outline-show-all)
			  (setq outline-hidden nil))
		  (progn
			(outline-hide-body)
			(setq outline-hidden t)))
	  (progn
		(set (make-local-variable 'outline-hidden) nil)
		(outline-show-all)))))

(use-package outshine
  :after (outline)
  :demand t
  :commands  (outshine-hook-function fn--evil->-handler fn--evil-<-handler)
  :hook (outline-minor-mode . outshine-mode)
  :bind (("<backtab>" . #'outshine-cycle-buffer))
  :config
  (setq outshine-use-speed-commands t)
  (setq outshine-fontify-whole-heading-line nil)
  (setq outshine-preserve-delimiter-whitespace t)

  (evil-define-key '(normal visual evilified) c++-mode-map (kbd ">") #'fn--evil->-handler)
  (evil-define-key '(normal visual evilified) c++-mode-map (kbd "<") #'fn--evil-<-handler)

  (defun fn--evil->-handler ()
	(interactive)
	(if (outline-on-heading-p)
		(outline-demote)
	  (call-interactively 'evil-shift-right)))

  (defun fn--evil-<-handler ()
	(interactive)
	(if (outline-on-heading-p)
		(outline-promote)
	  (call-interactively 'evil-shift-left))))

(use-package outline-ivy
  :after (outshine outline)
  :demand t
  :bind (:map evil-motion-state-map
			  ("gM" . 'ivy-outline))
  :config
  (defalias 'ivy-outline 'oi-jump)
  )

;;; code-folding

;;;; hideshow
(use-package hideshow
  :commands (fn-hs-toggle-all fn-hs-toggle-hiding)
  :demand
  :config

  (defun fn-hs-toggle-all ()
	(interactive)
	;; is local variable set?  no -> unhide all, set the variable
	;; yes -> read its state.
	;; if hidden == t, unhide all
	;; if hidden == nil, hide all
	(save-excursion
	  (save-restriction
		(if (local-variable-p 'hidden)
			(if (equal hidden t)
				(progn
				  (hs-show-all)
				  (setq hidden nil))
			  (progn
				(hs-hide-all)
				(setq hidden t)))
		  (progn
			(set (make-local-variable 'hidden) nil)
			(hs-show-all))))))

  (defun fn-hs-toggle-hiding ()
	(interactive)
	(save-excursion
	  (hs-toggle-hiding)))

  )

;;;; hideshow-org
(use-package hideshow-org
  :after (org hideshow)
  :commands hs-org/minor-mode
  :hook (python-mode . hs-org/minor-mode)

  :config
  ;; NOTE: kann weg?
  ;; (defvar hs-org/trigger-keys-block (list (kbd "<C-tab>"))
  ;; 	"The keys to bind to toggle block visibility.")
  ;; (defvar hs-org/trigger-keys-all (list [S-tab] [S-iso-lefttab] [(shift tab)] [backtab])
  ;; 	"The keys to bind to toggle all block visibility.")

  )
