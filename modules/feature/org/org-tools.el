;; modules/feature/org/org-tools.el    -*- lexical-binding: t; -*-

;;;###autoload
(defun +org-insert-heading ()
  (interactive)
  (org-insert-heading)
  (evil-insert 0))

;;;###autoload
(defun fn-org-refile (f &rest args)
  (interactive)
  (porg--with-org-agenda-files
   (apply f args)))

;; ---------------------------------------------------------
;; extract categories from org files  (-> :CATEGORY tag

;; (defun fn-org-extract-categories (files)
;; 	(interactive)
;; 	(setq fn-org-project-categories 'nil)
;; 	(dolist (agenda-file files)
;; 	  (set-buffer (find-file-noselect agenda-file))
;; 	  (setq fn-org-project-categories (append fn-org-project-categories (org-property-values "CATEGORY"))))
;; 	(delete-dups fn-org-project-categories)
;; 	)


;; (if (equal location "home")
;; 	  (fn-org-extract-categories (org-agenda-files))
;; 	(fn-org-extract-categories work-projects-agenda-files))

;; ;; parse file properties --
;; (defun jk-org-kwds ()
;;   "parse the buffer and return a cons list of (property . value)
;; from lines like:
;; #+PROPERTY: value"
;;   (org-element-map (org-element-parse-buffer 'element) 'keyword
;;     (lambda (keyword) (cons (org-element-property :key keyword)
;;                             (org-element-property :value keyword)))))

;; (defun jk-org-kwd (KEYWORD)
;;   "get the value of a KEYWORD in the form of #+KEYWORD: value"
;;   (cdr (assoc KEYWORD (jk-org-kwds))))

;; ;; parse file properties via regex --
;; (defun jk-get-file-keyword (KEYWORD)
;;   "get the value from a line like this
;; #+KEYWORD: value
;; in a file."
;;   (interactive)
;;   (let ((case-fold-search t)
;;         (re (format "^#\\+%s:[ \t]+\\([^\t\n]+\\)" KEYWORD)))
;;     (if (not (save-excursion
;;                (or (re-search-forward re nil t)
;;                    (re-search-backward re nil t))))
;;         (error (format "No line containing #+%s: value found" KEYWORD)))
;;     (match-string 1)))

;; ;; parse file, find CATEGORY properties --
;; (defun fn-get-property-keyword (KEYWORD)
;;   "get the value from a line like this
;; :CATEGORY: value
;; in a file."
;;   (interactive)
;;   (let ((case-fold-search t)
;;         (re (format "^:%s:[ \t]+\\([^\t\n]+\\)" KEYWORD)))
;;     (if (not (save-excursion
;;                (or (re-search-forward re nil t)
;;                    (re-search-backward re nil t))))
;;         (error (format "No line containing :%s: value found" KEYWORD)))
;;     (match-string 1)))


;; ;; ------------------------------------------------------
;; code-archive / library linkage
;; (defun fn-link-code-archive ()
;;   (interactive)
;;   (find-file (concat dir_archive "/"))
;;   (helm-projectile-find-file)
;;   )

;; -- template for implementing the above function, ........
;; (defun helm-projectile-find-file-dwim ()
;;   "Find file at point based on context."
;;   (interactive)
;;   (let* ((project-files (projectile-current-project-files))
;;          (files (projectile-select-files project-files)))
;;     (if (= (length files) 1)
;;         (find-file (expand-file-name (car files) (projectile-project-root)))
;;       (helm :sources (helm-projectile-build-dwim-source (if (> (length files) 1)
;;                                                             files
;;                                                           project-files))
;;             :buffer "*helm projectile*"
;;             :prompt (projectile-prepend-project-name "Find file: ")))))


;; ---------------------------------------------------------
;; automatically adjust task state from childen checkboxes

;; (defun fn-org-checkbox-todo ()
;; 	"Switch header TODO state to DONE when all checkboxes are ticked, to TODO otherwise"
;; 	(let ((todo-state (org-get-todo-state)) beg end)
;; 	  (unless (not todo-state)
;; 		(save-excursion
;; 		  (org-back-to-heading t)
;; 		  (setq beg (point))
;; 		  (end-of-line)
;; 		  (setq end (point))
;; 		  (goto-char beg)
;; 		  (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
;; 								 end t)
;; 			  (if (match-end 1)
;; 				  (if (equal (match-string 1) "100%")
;; 					  (unless (string-equal todo-state "DONE")
;; 						(org-todo 'done))
;; 					(unless (string-equal todo-state "TODO")
;; 					  (org-todo 'todo)))
;; 				(if (and (> (match-end 2) (match-beginning 2))
;; 						 (equal (match-string 2) (match-string 3)))
;; 					(unless (string-equal todo-state "DONE")
;; 					  (org-todo 'done))
;; 				  (unless (string-equal todo-state "TODO")
;; 					(org-todo 'todo)))))))))

;; (add-hook 'org-checkbox-statistics-hook 'fn-org-checkbox-todo)

;; ---------------------------------------------------------
(defvar fn--org-hide-drawers t)

(defun fn-org-cycle-hide-drawers (state)
  "Re-hide all drawers after a visibility state change."
  (when (and (derived-mode-p 'org-mode)
             (not (memq state '(overview folded contents))))
    (save-excursion
      (let* ((globalp (memq state '(contents all)))
             (beg (if globalp (point-min) (point)))
             (end (if globalp (point-max)
					(if (eq state 'children)
						(save-excursion (outline-next-heading) (point))
                      (org-end-of-subtree t)))))
        (goto-char beg)
        (while (re-search-forward org-drawer-regexp end t)
          (save-excursion
            (beginning-of-line 1)
            (when (looking-at org-drawer-regexp)
              (let* ((start (1- (match-beginning 0)))
                     (limit
                      (save-excursion
                        (outline-next-heading)
                        (point)))
                     (msg (format
                           (concat
                            "org-cycle-hide-drawers:  "
                            "`:END:`"
                            " line missing at position %s")
                           (1+ start))))
                (if (re-search-forward "^[ \t]*:END:" limit t)
					(outline-flag-region start (point-at-eol) t)
                  (user-error msg))))))))))

(defun fn-toggle-hide-drawers ()
  (interactive)
  (if fn--org-hide-drawers
	  (progn (message "Hiding drawers...")
			 (setf fn--org-hide-drawers nil))
	(progn (message "Showing drawers...")
		   (setf fn--org-hide-drawers t))))

(defun fn-org-cycle ()
  "Wrapper org-force-cycle-archived to achieve, on another key
than <tab>, the reverse behaviour w.r.t. :PROPERTIES: that <tab>
has."
  (interactive)
  (let ((fn--org-hide-drawers (not fn--org-hide-drawers)))
	(call-interactively 'org-force-cycle-archived)))

(defun fn--around-org-cycle-hide-drawers (orig-fun state &optional exceptions)
  (if fn--org-hide-drawers
	  (fn-org-cycle-hide-drawers state)
	(apply orig-fun state exceptions)))

;; ---------------------------------------------------------
;; (because it seems as if not all features are loaded initially - whyever that is...)
(defun reload-all-org-mode-buffers ()
  (dolist ($buf (buffer-list (current-buffer)))
    (with-current-buffer $buf
      (when (equal major-mode 'org-mode) (org-mode) )
      )))


;; ---------------------------------------------------------
;; window management
(defun fn-suppress-delete-other-windows (orig-fn &rest args)
  (cl-letf (((symbol-function 'delete-other-windows)
			 (symbol-function 'ignore)))
	(apply orig-fn args)))

(defun fn--org-window-handler (&rest args)
  "Handle the current org-window."
  (let* ((arg (car args))
		 (buf (cond ((stringp arg) (get-buffer-create arg))
					((bufferp arg) arg)
					(t (error "Invalid buffer %s" buf))))
		 (buf-name (buffer-name buf)))
	(cond ((s-prefix? "CAPTURE" buf-name)
		   (select-window
			(split-window (selected-window) -10)))
		  ((string=  " *Agenda Commands*" buf-name)
		   (select-window
			(split-window (selected-window))))
		  ((string= "*Org Agenda*" buf-name)
		   ;; just use current window for now, choose behaviour by type later
		   ;; (select-window
		   ;;  (split-window (selected-window)))
		   )
		  )
	(switch-to-buffer buf t t)
	(goto-char (point-max))
	nil))


;; (defun +org-anki-insert ()
;;   (interactive)

;;   ;; prompt for name
;;   ;; use name for inserting a heading thus named in level 2
;;   ;; Insert level 3 headings FRONT and BACK, one empty line apart each
;;   ;; use name and current level 4 heading below FRONT to insert heading level 3 "L1 - L2"

;;   )
