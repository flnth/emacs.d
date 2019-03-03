;; modules/core/general/config.el    -*- lexical-binding: t; -*-

;;;; mmm-mode
(use-package mmm-mode)

;;;; prog modes
(defun +general-prog-mode-config ()
  (setq truncate-lines t))
(add-hook #'prog-mode-hook #'+general-prog-mode-config)

;;;; pretty-printing
(setq eval-expression-print-level nil
	  eval-expression-print-length nil)

;;;; (directory) local variables
;; dir-locals

;; just mark them safe, always, for now
(setq enable-local-variables :all)

;; prevent them from being loaded on opening new files
;;  (-> use custom hook)
(setq enable-dir-local-variables nil)

(defun +utils-set-file-local-variable (var val dir &optional category)
  "Changes the value of file-local variable VAR to value VAL in
directory DIR. CATEGORY determines the car of the cell under
which the variable is being stored. Creates new class from
directory.

var			: interned symbol
val			: *
dir			: string
category	: interned symbol"
  (let* ((dir-sym (intern (s-collapse-whitespace (f-slash dir))))
		 (class-vars (cdr (assoc category (dir-locals-get-class-variables dir-sym))))
		 (ex-var-cell (assoc var class-vars)))
	(if ex-var-cell
		(setf (cdr ex-var-cell) val) 	; update
	  (if (null class-vars) 			; set
		  (setf class-vars `((,var . ,val)))
		(nconc class-vars `((,var . ,val)))))
	(dir-locals-set-class-variables dir-sym `((,category . ,class-vars)))
	(dir-locals-set-directory-class dir dir-sym)))

(cl-defmacro +utils-with-every-live-buffer-below (dir &rest body)
  `(dolist (buffer (buffer-list))
	 (with-current-buffer buffer
	   (when (and (buffer-file-name)
				  (s-prefix? ,dir default-directory))
		 ,@body))))

;; NOTE: "it's much faster to use buffer-local-value than with-current-buffer to
;; access the value of a variable in a buffer"
;; https://github.com/alphapapa/emacs-package-dev-handbook#best-practices
(defun +utils-update-dir-locals-below (dir)
  (+utils-with-every-live-buffer-below
   dir
   (hack-local-variables)))

;; on opening new files, make sure that dir-local variables are being checked
;; (remove-hook #'find-file-hook #'hack-local-variables)

;; ---------------------------------------------------------
;; -- deprecated?
(defun fn-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun fn-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (fn-reload-dir-locals-for-current-buffer)))))

;; (add-hook 'emacs-lisp-mode-hook
;;           (defun enable-autoreload-for-dir-locals ()
;;             (when (and (buffer-file-name)
;;                        (equal dir-locals-file
;;                               (file-name-nondirectory (buffer-file-name))))
;;               (add-hook (make-variable-buffer-local 'after-save-hook)
;;                         'fn-reload-dir-locals-for-all-buffer-in-this-directory))))
