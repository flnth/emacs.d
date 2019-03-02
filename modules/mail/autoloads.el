;; -*- lexical-binding: t -*-

;;;###autoload
(defun fn-mu4e-add-maildirs ()
  "Populates maildirs-extension-custom-list from directory
contents. Considers FN-MU4E-IGNORED-MAILDIRS,
FN-MU4E-FRONT-MAILDIRS. Extracts maildir subdirectories from
mu4e-contexts. Convention: every mu4e context name directly
corresponds to a maildir subdirectory. "
  (setq mu4e-maildirs-extension-custom-list nil)
  (let ((repos '("/indurad/" "/rwth/"))
		;; (repos (mapcar #'(lambda (context) (concat "/" (mu4e-context-name context) "/" )) mu4e-contexts))
		(repo-maildirs '()))
	;; -- populate
	(dolist (repo repos)
	  (setf repo-maildirs nil)
	  (let ((repo-abs-dir (concat mu4e-maildir repo)))
		(when (f-directory? repo-abs-dir)
		  (setf repo-maildirs
				(nconc (mapcar #'(lambda (s) (concat repo s)) (directory-files repo-abs-dir))
					   repo-maildirs))))

	  ;; -- filter (per regex)
	  (dolist (ignored fn-mu4e-ignored-maildirs)
		(setf repo-maildirs
			  (-filter (lambda (el) (not (string-match ignored el))) repo-maildirs)))

	  ;; -- sort (no regex)
	  (setf repo-maildirs (sort repo-maildirs 'string-collate-lessp))

	  (dolist (front fn-mu4e-front-maildirs)
		(when (-contains? repo-maildirs front)
		  (setf repo-maildirs
				(cons front (remove* front repo-maildirs)))))

	  (setf mu4e-maildirs-extension-custom-list (nconc repo-maildirs mu4e-maildirs-extension-custom-list)))))

;;;###autoload
(defun fn-mu4e-headers-quit-buffer ()
  (interactive)
  (kill-buffer))

;;;###autoload
(defun fn-mu4e-move-to-trash-at-point ()
  (let* ((msg (mu4e-message-at-point)))
	(mu4e~proc-move (plist-get msg :docid)
					(mu4e-get-trash-folder msg)
					"-N+S")))
