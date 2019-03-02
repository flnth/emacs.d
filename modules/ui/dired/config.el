;;; modules/ui/dired/config.el                   -*- lexical-binding: t; -*-

(message "access/config.el ...")

(use-package dired
  ;; :after (ivy ido counsel projectile counsel-projectile f magit)
  ;; :demand
  ;; :bind (:map dired-mode-map
  ;; 			  ("C-t" . tabbar-new-tab)
  ;; 			  )
  ;; :commands (dired)

  :config
  ;; (remove-hook 'dired-mode-hook #'+header-line-update )

  (defun +dired--header-line-update ()
	(setq header-line-format '(:eval (tabbar-line))))
  (add-hook #'dired-mode-hook #'+dired--header-line-update)

  ;; (define-key dired-mode-map (kbd "C-t") #'tabbar-new-tab )

  ;; TODO:  special fontify for dired tabbar

  )



