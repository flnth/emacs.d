;;; modules/lang/ert/config.el    -*- lexical-binding: t; -*-

(use-package ert
  :after (ui/access/access tabbar)
  ;; :demand
  :bind (:map ert-results-mode-map
			  ("q" . fn-close-window )
			  ("C-w" . fn-close-window)
			  )
  )

;;;; ert, debugging
;; TODO:  show results as quick-peek popup?

(defun +lisp-run-last-ert-test ()
  "Re-runs the last tests using the selector in
  ert--selector-history."
  (interactive)
  (let* ((cur-win (selected-window))
		 (stats (ert-run-tests-interactively (car ert--selector-history))))
	(delete-window (selected-window))
	(select-window cur-win)))


