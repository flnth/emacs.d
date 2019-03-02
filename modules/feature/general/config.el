;; modules/feature/general/config.el    -*- lexical-binding: t; -*-

(use-package gtypist-mode
  :commands (gtypist-mode)
  :load-path "packages"
  :init
  (setq auto-mode-alist
		(cons '("\\.typ\\'" . gtypist-mode) auto-mode-alist)))

