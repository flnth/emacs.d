;; -*- lexical-binding: t -*-

(defun fn--mu4e-adjust-headers ()
  "Temporarily adjust number of headers and the size of subject
header based on available space in the window showing
*mu4e-headers*."
  ;; reset header config
  (setq mu4e-headers-fields mu4e--headers-fields-original)

  ;;   - after a short amount of time
  (let ((reset-closure (let ((old-val mu4e-headers-fields))
						 (lambda ()
  						   (set 'mu4e-headers-fields old-val)))))
  	(run-with-timer 0.25 nil reset-closure))

  (let ((width 0)
		(win-width 0)
		(mu4e-header-win (get-buffer-window (get-buffer "*mu4e-headers*")))
		(mu4e-headers-fields-temp '() )
		(subject-passed nil))
	(when mu4e-header-win

	  ;; remove headers that don't fit (after subject)
	  (setf win-width (window-width mu4e-header-win))
	  (loop for header in mu4e-headers-fields
			do (progn
				 (when (and subject-passed
							(> (+ width (cdr header)) win-width))
				   (return))
				 (when (eq (car header) :subject)
				   (setf subject-passed t))
				 (setf width (+ width (cdr header)))
				 (message (format "%s" mu4e-headers-fields-temp))
				 (setf mu4e-headers-fields-temp (cons header mu4e-headers-fields-temp))
				 ))
	  (setf mu4e-headers-fields (reverse mu4e-headers-fields-temp))

	  ;; fill subject-width as much as possible
	  (setf (alist-get :subject mu4e-headers-fields)
	  		(+ (alist-get :subject mu4e-headers-fields)
	  		   (- win-width width 5))))))

(defvar fn-mu4e-header-view-subject-minimum 60)
(defvar mu4e--headers-fields-original
  '((:subject . 60)
	(:from . 35)
	(:date . 16)) )


(add-hook 'mu4e-headers-mode-hook #'fn--mu4e-adjust-headers)
