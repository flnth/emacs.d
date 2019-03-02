;; +sidebar.el
;; -*- lexical-binding: t -*-

;; TODO:  how to enforce / communicate dependencies in here?
;;        ... if at all?

(use-package dash
  :ensure t)

(use-package ht
  :ensure t)

(use-package mu4e-maildirs-extension
  :ensure t)

;; + all-the-icons require

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mail-sidebar-side-pane
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Constants

(defconst mail-sidebar-fixed-size t)
(defconst mail-sidebar-buffer-name "*Mail*")
(defconst mail-sidebar-autorefresh nil)
(defconst mail-sidebar-window-width 35)
(defconst mail-sidebar-window-position 'left)
(defconst mail-sidebar-window-fixed-size t)
(defconst mail-sidebar-display-action '(mail-sidebar-display-function))

;;;; Variables

(defvar mail-sidebar--buffer nil)
(defvar mail-sidebar--window nil)
(defvar mail-sidebar--autorefresh-timer nil)
(defvar mail-sidebar--after-init-hook nil)

;;;; Macros

(defmacro mail-sidebar--with-buffer (&rest body)
  (declare (indent 0) (debug t))
  `(let ((mail-sidebar--buffer (mail-sidebar-buffer--get)))
	 (unless (null mail-sidebar--buffer)
	   (with-current-buffer mail-sidebar--buffer
		 ,@body))))

(defmacro mail-sidebar--with-editing-buffer (&rest body)
  `(let (rlt)
	 (mail-sidebar--with-buffer
	   (setq buffer-read-only nil)
	   (setq rlt (progn ,@body))
	   (setq buffer-read-only t))
	 rlt))

(defmacro mail-sidebar--with-window (&rest body)
  (declare (indent 0) (debug t))
  `(save-selected-window
	 (mail-sidebar-window--select)
	 ,@body))

(defmacro mail-sidebar-buffer--with-resizable-window (&rest body)
  `(let (rlt)
	 (mail-sidebar--with-buffer
	   (mail-sidebar-buffer--unlock-width))
	 (setq rlt (progn ,@body))
	 (mail-sidebar--with-buffer
	   (mail-sidebar-buffer--lock-width))
	 rlt))

(defmacro mail-sidebar-make-executor (&rest fn-form)
  (let* ((get-args-fn
		  (lambda (sym) (or (plist-get fn-form sym) (lambda (&rest _)))))
		 (file-fn (funcall get-args-fn :sub-fn))
		 (dir-fn (funcall get-args-fn :dir-fn)))
	`(lambda (&optional arg)
	   (interactive "P")
	   (mail-sidebar-window--select)
	   (mail-sidebar--buffer--execute arg ,file-fn ,dir-fn))))

;;;; Functions

(defun mail-sidebar-no-fringes ()
  (set-window-fringes mail-sidebar--window 0 0))

(defun mail-sidebar-display-function (buffer _alist)
  (let ((window-pos (if (eq mail-sidebar-window-position 'left) 'left 'right)))
	(display-buffer-in-side-window buffer `((side . ,window-pos)))))

(defun mail-sidebar-buffer--newline-and-begin ()
  (newline)
  (beginning-of-line))

(defun mail-sidebar-buffer--lock-width ()
  (if mail-sidebar-window-fixed-size
	  (setq window-size-fixed 'width)))

(defun mail-sidebar-buffer--unlock-width ()
  (setq window-size-fixed nil))

(defun mail-sidebar-buffer--create ()
  (switch-to-buffer
   (generate-new-buffer-name mail-sidebar-buffer-name))
  (mail-sidebar-mode)
  (when (and (boundp 'linum-mode)
			 (not (null linum-mode)))
	(linum-mode -1))
  (current-buffer))

(defun mail-sidebar-buffer--insert-root ()
  (set-buffer mail-sidebar--buffer)
  (setq header-line-format nil))

(defun mail-sidebar-buffer--get (&optional init-p)
  (unless (equal (buffer-name mail-sidebar--buffer)
				 mail-sidebar-buffer-name)
	(setf mail-sidebar--buffer nil))
  (when (and init-p
			 (null mail-sidebar--buffer))
	(save-window-excursion
	  (setq mail-sidebar--buffer
			(mail-sidebar-buffer--create)))
	(mail-sidebar-buffer--refresh))
  mail-sidebar--buffer)

(defun mail-sidebar-window--init (window buffer)
  (mail-sidebar-buffer--with-resizable-window
   (switch-to-buffer buffer)
   (set-window-dedicated-p window t))
  window)

(defun mail-sidebar-window--exists-p ()
  (and (not (null (window-buffer mail-sidebar--window)))
	   (eql (window-buffer mail-sidebar--window) (mail-sidebar-buffer--get))))

(defun mail-sidebar-window--select ()
  (interactive)
  (let ((window (mail-sidebar-window--get t)))
	(select-window window)))

(defun mail-sidebar-window--create ()
  (let ((window nil)
		(buffer (mail-sidebar-buffer--get t)))
	(setq window
		  (select-window
		   (display-buffer buffer mail-sidebar-display-action)))
	(mail-sidebar-window--init window buffer)
	(mail-sidebar--attach)
	(mail-sidebar--reset-width)
	window))

(defun mail-sidebar-window--get (&optional auto-create-p)
  (unless (mail-sidebar-window--exists-p)
	(setf mail-sidebar--window nil))
  (when (and (null mail-sidebar--window)
			 auto-create-p)
	(setq mail-sidebar--window
		  (mail-sidebar-window--create)))
  mail-sidebar--window)

(defun mail-sidebar--detach ()
  (when mail-sidebar--autorefresh-timer
	(cancel-timer mail-sidebar--autorefresh-timer))
  (mail-sidebar--with-buffer
	(mail-sidebar-buffer--unlock-width))
  (setq mail-sidebar--buffer nil)
  (setq mail-sidebar--window nil))

(defun mail-sidebar--attach ()
  (when mail-sidebar--autorefresh-timer
	(cancel-timer mail-sidebar--autorefresh-timer))
  (when mail-sidebar-autorefresh
	(setq mail-sidebar--autorefresh-timer
		  (run-with-idle-timer 30 1 'mail-sidebar--do-autorefresh)))
  (setq mail-sidebar--buffer (get-buffer mail-sidebar-buffer-name))
  (setq mail-sidebar--window (get-buffer-window mail-sidebar--buffer))
  (mail-sidebar--with-buffer
	(mail-sidebar-buffer--lock-width)))

(defun mail-sidebar-util--set-window-width (window n)
  (let* ((w (max n window-min-width))
		 (w (if (window-system) (+ w 20) w)))
	(unless (null window)
	  (if (> (window-width) w)
		  (shrink-window-horizontally (- (window-width) w))
		(if (< (window-width) w)
			(enlarge-window-horizontally (- w (window-width))))))))

(defun mail-sidebar--set-window-width (width)
  (mail-sidebar--with-window
	(mail-sidebar-buffer--with-resizable-window
	 (mail-sidebar-util--set-window-width (selected-window) width))))

(defun mail-sidebar--reset-width ()
  (mail-sidebar--set-window-width mail-sidebar-window-width))

(defun mail-sidebar--open ()
  (mail-sidebar-window--get t))

(defun mail-sidebar--do-autorefresh ()
  (interactive)
  (when (and mail-sidebar-autorefresh (mail-sidebar-window--exists-p))
	(mail-sidebar-refresh)))

(defun mail-sidebar-refresh ()
  (interactive)
  (mu4e-update-mail-and-index t)
  (if (eq (current-buffer) (mail-sidebar-buffer--get))
	  (mail-sidebar-buffer--refresh)))

(defun mail-sidebar-buffer--refresh ()
  (mail-sidebar--with-editing-buffer
   (erase-buffer)
   (mail-sidebar-buffer--insert-root)
   (insert "\n\t[c] Compose a message\n")
   (run-hooks 'mu4e-main-mode-hook)
   (goto-char 0)))

;;;; Major mode definition

(defvar mail-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "g") 'mail-sidebar-refresh)
	(define-key map (kbd "q") 'mail-sidebar-hide)
	(define-key map (kbd "c") (lambda () (interactive) (mu4e-compose 'new)))
	map))

(define-derived-mode mail-sidebar-mode special-mode "Mail"
  (setq indent-tabs-mode nil)
  (setq buffer-read-only t)
  (setq truncate-lines -1)
  (setq-local mode-line-format nil)
  (if (fboundp 'electric-indent-local-mode)
	  (electric-indent-local-mode -1)
	(add-hook 'electric-indent-functions
			  (lambda (arg) 'no-indent) nil 'local)))

;;;###autoload
(defun mail-sidebar-show ()
  (interactive)
  (let ((cw (selected-window)))
	(mail-sidebar--open))
  (mail-sidebar-window--select))

;;;###autoload
(defun mail-sidebar-hide ()
  (interactive)
  (if (mail-sidebar-window--exists-p)
	  (delete-window mail-sidebar--window)))

;;;###autoload
(defun mail-sidebar-toggle ()
  (interactive)
  (if (mail-sidebar-window--exists-p)
	  (mail-sidebar-hide)
	(mail-sidebar-show)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  hooks
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(advice-add #'mail-sidebar-window--select :after #'mail-sidebar-no-fringes)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mail keybindings
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key [f9] 'mail-sidebar-toggle)

(defalias 'mail 'mail-sidebar-toggle)

(provide 'sidebar)

