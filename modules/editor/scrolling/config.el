;; modules/editor/scrolling/config.el    -*- lexical-binding: t; -*-


(defun +ui--on-window-size-changed-update-scroll (frame)
  (interactive)
  (cl-loop for win in (window-list frame) do
		   (set-window-parameter win 'scroll-height
								 (max 1 (/ (1- (window-height win)) 4)))))
(add-hook 'window-size-change-functions #'+ui--on-window-size-changed-update-scroll)

;;;###autoload
(defun scroll-down-half ()
  (interactive)
  (let ((h (window-parameter (selected-window) 'scroll-height))
		(pos (window-start)))
	(if (or (eq pos (point-min))
			(eq pos (point-max)))
		;; at end or beginning of buffer:  just move cursor
		(next-line h)
	  ;; somewhere inside buffer: move text
	  (scroll-up h))))

;;;###autoload
(defun scroll-up-half ()
  (interactive)
  (let ((h (window-parameter (selected-window) 'scroll-height))
		(pos (window-start)))
	(if (or (eq pos (point-min))
			(eq pos (point-max)))
		;; at end or beginning of buffer:  move cursor
		(previous-line h)
	  ;; somewhere inside buffer:  move text
	  (scroll-down h))
	))

(global-set-key [prior] 'scroll-up-half)
(global-set-key [next] 'scroll-down-half)
(define-key evil-motion-state-map (kbd "C-u") 'scroll-up-half)
(define-key evil-motion-state-map (kbd "C-d") 'scroll-down-half)
;; half-page scrolling  (remove run-with-timer?)
(run-with-timer 2 nil #'(lambda ()
						  (setq evil-scroll-count 15)))
