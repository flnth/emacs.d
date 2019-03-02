;; modules/feature/magit/quick.el    -*- lexical-binding: t; -*-

;;;###autoload
(defun +magit-quick--remove-trailing-newline (string)
  (replace-regexp-in-string "\n$" "" string))

;;;###autoload
(defun +magit-quick--print-process-output (process event)
  "Provide feedback about a finished git process."
  (princ (format "git: %s" (+magit-quick--remove-trailing-newline event)))
  ;; TODO:  parse contents of *git*, give meaningful feedback.
  ;; TODO:  if successful, refresh magit-status buffer e.g. via (magit-status-refresh-buffer)?
  )

;;;###autoload
(defun +magit-quick-commit ()
  "Prompt for brief commit message in minibuffer, then commit."
  (interactive)
  (let ((projectile-require-project-root t)
		(default-directory (projectile-project-root)))
	(if default-directory 				; TODO: check for things staged
		(let* ((commit-message (read-from-minibuffer "commit message: ")) ;; TODO: check for empty commit message
			   (git-process (make-process :name "git"
										  :buffer (get-buffer-create "*git*")
										  :command `("git" "commit" "-m" ,commit-message))))
		  (set-process-sentinel git-process '+magit-quick--print-process-output))))
  (message "Not in a projectile repository."))

;; TODO:  a hotkey for quickly showing staged changes, somewhere
;; TODO:  a command to quickly stage a selected region  (+ committing it asap?)

;;;###autoload
(defun +magit-quick-add-untracked (arg)
  (interactive "p")
  ;; --default: project-wide, if possible
  (if (and (= arg 1)
		   (projectile-project-root))
	  (let ((default-directory (projectile-project-root)))
		(ivy-read
		 "add untracked: "
		 (magit-untracked-files)
		 :action 'magit-stage-file))

	;; --prefix argument: untracked from current buffer's directory downward, only
	(progn
	  (ivy-read
	   "add untracked: "
	   (magit-untracked-files)
	   :action 'magit-stage-file))))

(provide 'feature/magit/+quick)
