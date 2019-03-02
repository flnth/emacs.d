;; modules/ui/tty-colors/config.el       -*- lexical-binding: t; -*-

(use-package ui/tty-colors/tty-colors
  :after (faces)
  :demand
  :config
  (add-hook 'after-make-frame-functions
			   (lambda (new-frame)
				 (let ((current-frame (selected-frame)))
				   (select-frame new-frame)
				   (if (display-graphic-p)
					   ;; opening a gui frame wrecks tty colors -> fix
					   (run-with-idle-timer 1 nil '+tty-colors-fix-tty-colors)
					 (progn
					   (+tty-colors-define-standard-colors)
					   (+tty-colors-define-modified-colors)))
				   (select-frame current-frame))))
  )

