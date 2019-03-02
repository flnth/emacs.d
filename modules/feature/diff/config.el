;; modules/feature/diff/config.el    -*- lexical-binding: t; -*-


;;; ediff

(use-package ediff
  :commands (ediff-write-merge-buffer ediff)
  :config
  ;; (add-hook 'ediff-mode-hook 'fn-ediff-hook)
  (defun ediff-write-merge-buffer ()
	(let ((file ediff-merge-store-file)
		  (set-buffer ediff-buffer-C)
		  (write-region (point-min) (point-max) file)
		  (message "Merge buffer saved in: %s" file)
		  (set-buffer-modified-p nil)
		  (sit-for 1))))

  (setq ediff-quit-hook 'kill-emacs
		ediff-quit-merge-hook 'ediff-write-merge-buffer)

  (setq ediff-quit-hook nil
		ediff-quit-merge-hook nil))


;; (defun fn-ediff-hook ()
;;   (ediff-setup-keymap)
;;   (define-key ediff-mode-map "j" 'ediff-next-difference)
;;   (define-key ediff-mode-map "k" 'ediff-previous-difference))
