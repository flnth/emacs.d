

(defun pm--ensure-dirs-exist (&rest dirs)
  (dolist (dir dirs)
	(make-directory dir t)))


(provide 'pm-utils)


