;; modules/feature/history/config.el    -*- lexical-binding: t; -*-


;;;; window-history
(use-package whist
  :load-path "packages/whist.el"
  :bind (("<C-comma>" . 'whist-go-back)
		 ("<C-period>" . 'whist-go-forward)
		 ("C-," . 'whist-go-back)
		 ("C-." . 'whist-go-forward)
		 :map evil-normal-state-map
		 ("C-," . 'whist-go-back)
		 ("C-." . 'whist-go-forward))
  )

;;;; buffer edit-history
;; -- edit-history in current buffer
(defhydra hydra-last-change (:hint nil :head-hint nil)
  " "
  (";" (goto-last-change 100))
  ("," (goto-last-change-reverse -100)))
(define-key evil-normal-state-map "g;" (intern "hydra-last-change/lambda-;"))

;; (define-key evil-normal-state-map "g," (intern "hydra-last-change/lambda-,"))
