;;; +header-line.el
;;
;; Filename: +header-line.el
;;
;; Copyright (C) 2018 Florian Thevißen
;;
;; Description: Emacs header-line formatting tools
;; Author: Florian Thevißen <mail@florian-thevissen.de>
;; Keywords: lisp, tools
;; Created: Sun Oct 28 17:15:36 2018 (+0100)
;; Version: 0.1.0
;; Package-Required: TODO
;; URL: TODO
;;
(message "loading +header-line ...")

(defface header-line-face
  '((t (:foreground "#000000" :background "tan1")))
  "Face other header faces inherit from by default.")

(defface header-line-buffer-name-face
  '((t (:foreground "#000000" :background "tan1")))
  "Face used for buffer name in header-line.")

(defface header-line-directory-face
  '((t (:foreground "#000000" :background "tan1")))
  "Face used for directory name in header-line.")

(defface header-line-project-face
  '((t (:foreground "#000000" :background "tan1")))
  "Face used for project-face in header-line.")

(defface header-line-code-completion-face
  '((t (:foreground "#000000" :background "tan1")))
  "Face used for code-completion indicators.")

(defface header-line-warning-face
  '((t (:foreground "#000000" :background "tan1")))
  "Face used for warnings.")

(defvar header-line-left-width 50
  "The width of the left-aligned part of the tabbar.
  If necessary, content is truncated to make it fit this width.")

(defsubst +header-line-format-left (&optional max-width)
  "Returns fontified header-line string for the left-aligned part
of the header-line. It may be as large as MAX-WIDTH without
truncation, though for small window-sizes this may not be met."
  (let* ((ret (string))
  		 (max-width (if max-width max-width header-line-left-width))
  		 (proj (or (pjson-current-project)
  		 		   (f-filename (or (projectile-project-root)
								   (string)))))
  		 (dir (or (concat (when proj (concat " "))
  		 				  (s-chop-prefix (projectile-project-root) default-directory))
  		   		  (string)))
  		 (filename (or (ignore-errors (concat (f-filename (buffer-file-name))
  		   									  (when (buffer-modified-p) (string ?*))))
  		   			   (string))))
  	(when proj
  	  (put-text-property 0 (length proj) 'face 'header-line-project-face proj))

  	(put-text-property 0 (length filename) 'face 'header-line-buffer-name-face filename)
  	(put-text-property 0 (length dir) 'face 'header-line-directory-face dir)

  	;; truncate dir string if necessary:
  	(setf ret (concat proj dir filename))
  	(let ((len (length ret)))
  	  (when (>= len max-width)
  		(setf ret
  			  (concat proj
  					  (propertize (+header-line--truncate-dir dir
  															  (- max-width (length proj) (length filename)))
  								  'face 'header-line-directory-face)
  					  filename)))

	  ;; fill with spaces
	  (setf len (length ret))
	  (when (< len max-width)
		(setf ret (concat ret (propertize (make-string (- max-width len) ? )
										 'face 'header-line-face )) ))
	  ret)))

(defsubst +header-line-format-right ()
  (let* ((ycmd-active (ignore-errors ycmd-mode))
		 (lsp-active (ignore-errors lsp-mode))
		 (ycmd-string (if ycmd-active
						  (propertize "ycmd"
									  'face 'header-line-code-completion-face)
						""))
		 (lsp-string (if lsp-active
						 (propertize "lsp"
									 'face 'header-line-code-completion-face)
					   ""))
		 ;; (ycmd-active (boundp 'ycmd-active))
		 ;; (lsp-active (boundp '(lsp-active)))
		 )
	(concat ycmd-string lsp-string)))

(defun +header-line--truncate-dir (dir width)
  (let ((continue-flag t)
		(width (- width 3)))
	(while continue-flag
	  (let ((chopped (s-split-up-to "/" dir 1)))
		(if (cdr chopped)
			(progn
			  (setf dir (cadr chopped))
			  (setf continue-flag (< width (length dir))))
		  (setf dir (car chopped))
		  (setf continue-flag nil))))
	(concat " .." dir)))

(defstruct +header-line--window-state
  (header-line-cache (string))
  (last-buffer-seen nil)
  (dirty t)
  (last-buffer-seen-modified )
  )

;; window-configuration-change-hook:  detect buffer change, and/or create new window-parameter
(defun +header-line--on-window-configuration-changed ()
  "Checks if header-line state should be updated."
  (dolist (win (window-list nil nil))
  	(let ((state (window-parameter win 'header-line-state))
  		  (win-buf (window-buffer win)))

  	  ;; create window state parameter if necessary
  	  (when (null state)
  		(setf state (set-window-parameter (selected-window) 'header-line-state
  										  (make-+header-line--window-state))))

  	  ;; check if setting dirty flag is necessary
  	  (when (not (eq win-buf (+header-line--window-state-last-buffer-seen state)))
  		(setf (+header-line--window-state-dirty state) t)
  		(setf (+header-line--window-state-last-buffer-seen state) win-buf)
  		)))
  )
(add-hook #'window-configuration-change-hook #'+header-line--on-window-configuration-changed)

(setq +header-line--dir-locals-changed nil)

;; hack-dir-locals called somewhere:  schedule update on all header lines
(defun +header-line--on-hack-dir-locals ()
  (setq +header-line--dir-locals-changed t))
(add-hook #'hack-dir-local-variables #'+header-line--on-hack-dir-locals)

(defun +header-line-format ()
  "Returns fontified header-line string. Does a full
recomputation only if necessary, which is if either of those
conditions is true:
   - buffer changed
   - buffer-modified changed
   - file-locals changed"

  ;; create local cache/window state if necessary
  (when (null (window-parameter (selected-window) 'header-line-state))
	(setf (window-parameter (selected-window) 'header-line-state)
		  (make-+header-line--window-state)))

  ;; +header-line--dir-locals-changed? -> schedule re-computation of header lines
  ;; on all frames
  (when +header-line--dir-locals-changed
  	(dolist (frame (frame-list))
  	  (dolist (win (window-list frame nil))
  		(let ((state (window-parameter win 'header-line-state)))
  		  (setf (+header-line--window-state-dirty state) t)
		  (+header-line-update)
  		  (setf (+header-line--window-state-dirty state) nil))))
  	(setf +header-line--dir-locals-changed nil))

  ;; window dirty or modified changed?
  (let ((state (window-parameter (selected-window) 'header-line-state)))
	(if (and
		 (eq (buffer-modified-p)
		 	 (+header-line--window-state-last-buffer-seen-modified state))
		 (not (+header-line--window-state-dirty state)))
		;; -- return cached value ----
		(progn
		  (setf (+header-line--window-state-dirty state) nil)
		  (+header-line--window-state-header-line-cache state))
	  ;; ---- recompute -----------
	  (setf (+header-line--window-state-dirty state) nil)
	  (setf (+header-line--window-state-last-buffer-seen-modified state) (buffer-modified-p))
	  (let ((left-aligned (string))
			(spacer (string ? ))
			(right-aligned (string)))

		;; -- left-aligned
		(setf left-aligned (+header-line-format-left))

		;; -- right-aligned
		(setf right-aligned (+header-line-format-right))
		;; -- combine
		(put-text-property 0 (length spacer)
						   'display `(space :align-to
											,(- (window-width) (length right-aligned)))
						   spacer)
		(put-text-property 0 (length spacer)
						   'face 'header-line-face spacer)

		(setf right-aligned (concat right-aligned (propertize "  " 'face 'header-line-face)))
		(setf (+header-line--window-state-header-line-cache state)
		  	  (concat left-aligned spacer right-aligned))
		))))

(defun +header-line-update ()
  (setq header-line-format (+header-line-format)))

(message "...done!")

(provide 'ui/header-line/+header-line)
