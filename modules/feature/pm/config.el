;;; modules/feature/pm/config.el                   -*- lexical-binding: t; -*-

(message "feature/pm/config.el ...")

(use-package pm
  ;; :load-path (concat (getenv "DIR_SYSTEM") "/emacs/modules/feature/pm/")
  :load-path (lambda () (concat (getenv "DIR_SYSTEM") "/emacs/modules/feature/pm/"))
  ;; :after (ivy ido counsel projectile counsel-projectile f magit)
  ;; :demand
  :init

  :config
  ;; (add-hook 'find-file-hook #'pm-locals-update-here)
  )


