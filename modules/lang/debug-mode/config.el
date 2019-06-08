;;; modules/lang/debug-mode/config.el    -*- lexical-binding: t; -*-

(use-package debug
  :after (ui/access/access)
  ;; :demand
  :bind (:map debugger-mode-map
			  ("gb" . acc-ido-switch-buffer)
			  ("0" . evil-beginning-of-line)
			  ("$" . evil-end-of-line)
			  )

  )







