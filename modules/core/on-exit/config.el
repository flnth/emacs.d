;; modules/core/on-exit/config.el    -*- lexical-binding: t; -*-


(defun close-all-major-mode-buffers (mode)
  (dolist ($buf (buffer-list (current-buffer)))
    (with-current-buffer $buf
      (when (member major-mode mode) (kill-buffer))
      )))

(defun close-all-c-buffers ()
  (interactive)
  (close-all-major-mode-buffers '(c-mode c++-mode))
  )

(defun close-all-elisp-buffers ()
  (interactive)
  (close-all-major-mode-buffers '(emacs-lisp-mode))
  )

(defun close-all-org-buffers ()
  (interactive)
  (close-all-major-mode-buffers '(org-mode))
  )

(add-hook 'kill-emacs-hook 'close-all-c-buffers)
