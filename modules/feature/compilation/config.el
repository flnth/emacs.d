;; modules/feature/compilation/config.el    -*- lexical-binding: t; -*-

;;; pjson
;; project configuration

(require 'f)
(require 'projectile)
(require 'pulse)
(use-package asoc
  :load-path "packages/asoc.el")

(use-package feature/compilation/pjson
  :after (f projectile asoc)
  :commands (pjson-init
			 pjson-ivy-compile-target
			 pjson-compile-last-target
			 pjson-ivy-cmake-gui)

  :bind (("C-b" . pjson-ivy-compile-target)
		 ("C-S-b" . pjson-compile-last-target)
		 :map evil-normal-state-map
		 ("C-b" . pjson-ivy-compile-target)
		 ("C-b" . pjson-ivy-compile-target))

  :init
  (spacemacs/set-leader-keys "cg" 'pjson-ivy-cmake-gui)
  (require 'feature/cmake/cmake)

  :config
  (advice-add 'compilation-read-command :around #'pjson--compilation-read-command)
  ;; (advice-add 'compilation-start :around #'pjson--compilation-start-around)
  (advice-remove 'compilation-start #'pjson--compilation-start-around)
  (advice-add 'recompile :around #'pjson-recompile-around)
  )


;;; compilation

;; SPC-] to toggle compilation buffer in current frame without selecting it
;; TODO:  maybe, for all "these" popup window (or just some?), check size window
;;        would use in frame. If size very small, donot popup, but switch-buffer.

;;;###autoload
(defun fn-toggle-compilation-window ()
  "Toggle display of compilation window in current frame."
  (interactive)
  (if (null (get-buffer "*compilation*"))
	  (message "%s" "No compilation buffer." )
	(let ((cwindow (get-buffer-window "*compilation*")))
	  (if (null cwindow)
		  (pop-to-buffer "*compilation*")
		(delete-window cwindow)))))

(spacemacs/set-leader-keys "]" 'fn-toggle-compilation-window)

;;;; Next-error wrap

(defun fn-next-error ()
  (interactive)
  (condition-case nil
	  (compilation-next-error 1)
	('user-error
	 (progn
	   (fn-flash-modeline)
	   (compilation-next-error 1 nil (point-min)))))
  (compilation-display-error)
  )

(defun fn-previous-error ()
  (interactive)
  (condition-case nil
	  (compilation-next-error -1)
	('user-error
	 (fn-flash-modeline)
	 (compilation-next-error -1 nil (point-max))))
  (compilation-display-error)
  )

(defun fn-flash-modeline ()
  (interactive)
  (let ((mode-line-color (face-background 'mode-line)))
	(set-face-background 'mode-line "#20b2aa")
	(sit-for 0.05)
	(set-face-background 'mode-line mode-line-color)))
;; .dir_locals.el example snippet
;; ((nil . ((multi-compile-alist (c++-mode . (("cpp-run" "make -j5" "/home/fthevissen/software/zeal/build")
;; 											   ("cpp-run2" "make -j5" ))
;; 											)
;; 							  ))
;; 	  ))


;;;; remember previous multi-compile choice
;; currently: non inter-session persistent, non-project specific

;; TODO: store fm-last-multic-compile-cmd in 1) file, 2) associate with project
;;       so I can just re-compile using the last-used command in a project (whatever that may be)

;; NOTE: Argument against that would be; I don't want to have to constantly think about where I am
;;       when I start a compilation. So, do NOT have it context-specific, but just do rerun last command.

(defun fn-find-compile-cmd-in (key lst)
  "Recursively search through the passed lst with format of
multi-compile-alist, and return compile command and/or directory.
Returns either nil for no match, cmd, or '(cmd dir)"
  (interactive)
  (cl-labels ((lin-search (lambda (key ilst)
					  (cond ((null ilst) nil)
							(t (let (( match (assoc key (cdr (car ilst)))))
								 (if (null match)
									 (lin-search key (cdr ilst))
								   match)))))))
	(cdr (lin-search key lst))))

(setq fn-last-multi-compile-cmd nil)

(defun fn-multi-compile-run ()
  "Runs multi-compile-run, and stores all runtime information in
variable  fn-last-multi-compile-cmd  afterwards (either cmd or cmd+dir)."
  (interactive)
  (multi-compile-run)
  (setq fn-last-multi-compile-cmd
  		(fn-find-compile-cmd-in (car multi-compile-history)
  								multi-compile-alist))
  ;; (let* (( cmd (fn-find-compile-cmd-in (car multi-compile-history)
  ;; 									   multi-compile-alist))
  ;; 		 (default-directory (if (listp cmd) (cadr cmd) default-directory ))
  ;; 		 )
  ;; 	(when (listp cmd) (setq cmd (car cmd)))
  ;; 	(compilation-start cmd)
  )

(defun fn-multi-compile-rerun ()
  (interactive)
  (if (not (null fn-last-multi-compile-cmd))
	  (let* (
			 (default-directory (if (listp fn-last-multi-compile-cmd) (cadr fn-last-multi-compile-cmd) default-directory))
			 (cmd (if (listp fn-last-multi-compile-cmd) (car fn-last-multi-compile-cmd) fn-last-multi-compile-cmd))
			 ;; (setq shell-command-switch "-ic")
			 )
		;; (prin1 default-directory)
		(compilation-start cmd)
		)
	(fn-multi-compile-run))
  )


;;; compilation window

;; Prevent compilation windows to pop-up
(defadvice compilation-start
	(around inhidbit-display
			(command &optional mode name-function highlight-regexp))
  (flet ((display-buffer))
	(fset 'display-buffer 'ignore) ad-do-it))
(ad-activate 'compilation-start)
;; (ad-deactivate 'compilation-start)


;;;; Highlighting

;; (add-hook 'compilation-mode-hook 'my-compilation-hook)


;; (setq compilation-exit-message-function 'compilation-exit-autoclose)

(defvar fn-comp-buffer-overlays ())

(defun fn-delete-this-overlay(overlay is-after begin end &optional len)
  (delete-overlay overlay)
  (setq fn-comp-buffer-overlays (delete overlay fn-comp-buffer-overlays))
  )

(defun fn-highlight-current-line (face)
  "Highlight current line with given face.
Sample call:  (fn-highlight-current-line '(background-color . \"red))"
  (interactive)
  (let* ((beg (progn (beginning-of-line) (point)))
		 (end (progn (forward-line 1) (point)))
		 (overlay (make-overlay beg end)))
	(overlay-put overlay 'face face)
	(overlay-put overlay 'modification-hooks (list 'fn-delete-this-overlay))
	overlay
	)
  )

(defun fn-remove-highlight-current-line ()
  (interactive)
  (delete-overlay (car (overlays-at (point)))))

(defface fn-comp-buffer-error-highlight
  '((t (:background "8c8ac2")))
  "Face used to highlight the current line in the compilation buffer."
  :group 'basic-faces)

(defun fn-comp-buffer-delete-overlays ()
  "Delete all overlays in the compilation buffer."
  (while fn-comp-buffer-overlays
	(delete-overlay (pop fn-comp-buffer-overlays)))
	)

(defun fn-comp-buffer-highlight-next-error ()
  "Highlights the next error-error line."
  (interactive)
  (with-current-buffer compilation-last-buffer
	(let* ((new-overlay (fn-highlight-current-line 'fn-comp-buffer-error-highlight)))
	  (fn-comp-buffer-delete-overlays)
	  (push new-overlay fn-comp-buffer-overlays)
	  )
	)
  )

(add-hook 'next-error-hook 'fn-comp-buffer-highlight-next-error)

(defun fn-comp-on-compilation-finish (buf message)
  (message "%s %s"
		   (propertize "Compilation finished: " 'face '(:foreground "red"))
		   (propertize (s-chomp message) 'face '(:foreground "white"))))

(add-hook 'compilation-finish-functions 'fn-comp-on-compilation-finish)

;; (defun delete-all-overlays ()
;;   "Delete all overlays"
;;   (while all-overlays
;;     (delete-overlay (car all-overlays))
;;     (setq all-overlays (cdr all-overlays))))

;; (defun highlight-error-lines(compilation-buffer process-result)
;;   (interactive)
;;   (delete-all-overlays)
;;   (condition-case nil
;;       (while t
;;         (next-error)
;;         (highlight-current-line))
;;     (error nil)))
;; (setq compilation-finish-functions 'highlight-error-lines)


(define-key compilation-mode-map (kbd "C-n") 'fn-next-error)
(define-key compilation-mode-map (kbd "C-p") 'fn-previous-error)

(require 'evil-collection-compile)
(evil-collection-compile-setup)
(define-key compilation-mode-map (kbd "TAB") 'compilation-display-error)
(define-key compilation-mode-map (kbd "M-n") (lambda () (interactive) (compilation-next-file 1) (compilation-display-error) ))
(define-key compilation-mode-map (kbd "M-p") (lambda () (interactive) (compilation-previous-file 1) (compilation-display-error) ))

(define-key compilation-mode-map (kbd "q") 'delete-window )
