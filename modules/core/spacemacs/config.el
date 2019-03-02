;; modules/core/spacemacs/config.el    -*- lexical-binding: t; -*-

;; -- turn off which-key ---
(run-with-timer 2 nil
				'(lambda ()
				   (spacemacs/toggle-which-key-off)))
(spacemacs/toggle-which-key-off)

;; disable line truncation
(spacemacs/toggle-truncate-lines-on)

;; disable current line highlighting
(spacemacs/toggle-highlight-current-line-globally-off)
(spacemacs/toggle-aggressive-indent-globally-on)

;; disable evil-escape
(evil-escape-mode -1)

;; disable aggressive indent thing
(aggressive-indent-global-mode -1)

;; eval flashing
(eval-sexp-fu-flash-mode 1)

