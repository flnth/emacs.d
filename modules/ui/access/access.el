;;; access.el                                         -*- lexical-binding: t -*-
;;
;; Filename: access.el
;;
;; Copyright (C) 2018 Florian Thevißen
;;
;; Description: Emacs access tools
;; Author: Florian Thevißen <mail@florian-thevissen.de>
;; Keywords: lisp, tools
;; Created: Sun Oct 28 17:15:36 2018 (+0100)
;; Version: 0.1.0
;; Package-Required: TODO
;; URL: TODO
;;
;; -----------------------------------------------------------------------------
;;; Commentary
;;
;; Custom tools for accessing files/buffers/commands, and performing actions on
;; them.
;;
;; -----------------------------------------------------------------------------
(message "loading access ...")

;; TODO:  find better solution for this fuck, doesn't work
(require 'window-purpose)
(require 'window-purpose-switch)

;; =========================================================
;; recency-sorting for selecting projectile projects

(setq +access--projects-accessed-repo
	  (pcache-repository "projects-accessed"))

;; TODO: use ttl property of pcache-entries instead?
;; remove entries older than +access--projects-accessed-oldest days
(setq +access--projects-accessed-oldest 30)
(progn
  (let ((max-age (float (* +access--projects-accessed-oldest 24 60 60)))
		(cur-time (float-time (current-time))))
	(map-apply (lambda (key val)
				 (when (> (- cur-time (oref val :value))
						  max-age)
				   (pcache-invalidate +access--projects-accessed-repo key)))
			   (oref +access--projects-accessed-repo :entries)))
  nil)

(defun +access--sort-projects-binary-predicate (p1 p2)
  "p1 > p2?"
  (let ((atime1 (pcache-get +access--projects-accessed-repo
							(intern (f-slash (f-short p1)))))
		(atime2 (pcache-get +access--projects-accessed-repo
							(intern (f-slash (f-short p2))))))
	(cond
	 ((null atime1) nil)
	 ((null atime2) t)
	 ((> atime1 atime2) t))))

(defun +access--project-accessed ()
  "Changes access-time in repo."
  (pcache-put +access--projects-accessed-repo
  			  (intern (f-slash (f-short (projectile-project-root))))
  			  (float-time (current-time)))
  )
(add-hook #'magit-post-refresh-hook #'+access--project-accessed)
(add-hook #'magit-create-buffer-hook #'+access--project-accessed)
(add-hook #'magit-post-commit-hook #'+access--project-accessed)
(advice-add #'counsel-projectile-switch-project-action :after
			(lambda (arg) (+access--project-accessed)))


;;;; projects


(defun acc-counsel-projectile-switch-project-action (project)
  "Jump to a file or buffer in PROJECT."
  (let ((projectile-switch-project-action
         (lambda ()
           (acc-counsel-projectile-magit ivy-current-prefix-arg))))
    (counsel-projectile-switch-project-by-name project)))

(defun acc-counsel-projectile-switch-project (&optional default-action)
  "Switch project. Like `counsel-projectile-switch-project', but
uses `acc-counsel-projectile-magit' instead of counsel-projectile"
  (interactive)
  (ivy-read (projectile-prepend-project-name "Switch to project: ")
            (if counsel-projectile-remove-current-project
                (projectile-relevant-known-projects)
              projectile-known-projects)
            ;; :preselect (and (projectile-project-p)
            ;;                 (abbreviate-file-name (projectile-project-root)))
            :action (or (and default-action
                             (listp counsel-projectile-switch-project-action)
                             (integerp (car counsel-projectile-switch-project-action))
                             (cons (counsel-projectile--action-index
                                    default-action
                                    counsel-projectile-switch-project-action)
                                   (cdr counsel-projectile-switch-project-action)))
                        #'acc-counsel-projectile-switch-project-action)
            :require-match t
            :sort counsel-projectile-sort-projects
            :caller 'counsel-projectile-switch-project))

(defun acc-counsel-projectile-action-magit (name)
  "Switch to buffer or find file named NAME, or open magit status
buffer if NAME is '<magit status>'."
  (cond
   ((and (string= name "▎ magit status"))
	(magit-status))
   ((member name counsel-projectile--buffers)
	(counsel-projectile-switch-to-buffer-action name))
   (t (counsel-projectile-find-file-action name))))

(defun acc-counsel-projectile-magit (&optional arg)
  "Jump to a buffer or file in the current project.

With a prefix ARG, invalidate the cache first.

If not inside a project, call `counsel-projectile-switch-project'.

If applicable, first item is magit status buffer."
  (interactive "P")
  (if (and (eq projectile-require-project-root 'prompt)
           (not (projectile-project-p)))
      (counsel-projectile-action-switch-project)
    (projectile-maybe-invalidate-cache arg)
    (ivy-read (projectile-prepend-project-name "Load buffer or file: ")
              ;; We use a collection function so that it is called each
              ;; time the `ivy-state' is reset. This is needed for the
              ;; "kill buffer" action.
			  #'(lambda (&rest _)
				  (append
				   (when (magit-toplevel) '("▎ magit status"))
				   (counsel-projectile--project-buffers-and-files)))
              ;; :matcher #'counsel-projectile--matcher
              :require-match t
              :action #'acc-counsel-projectile-action-magit
              :keymap counsel-projectile-map
              :caller 'counsel-projectile)))


;;;; buffers and files

(defface acc-rg-line-number-face
  '((t ()))
  "Face used to highlight line numbers in rg output.")

(defface acc-rg-path-face
  '((t ()))
  "Face used to highlight the path in rg output.")

(defface acc-rg-filename-face
  '((t ()))
  "Face used to highlight the filename in rg output.")

(defmacro acc-counsel-rg-transformer (&optional buf major-mode)
  "Create a transformer which uses temporary buffer to fontify."
  `(lambda (str)
	 (if (string-match "\\`\.\\([^:]+\\/\\)?\\([^:]*\\):\\([^:]+\\):\s*\\(.*\\)" str)
  		 (concat
  		  (format "%-6s %-40s "
  				  (propertize (substring str (match-beginning 3) (match-end 3))
  							  'face 'acc-rg-line-number-face)
  				  (concat
				   (when (match-beginning 1)
  					 (propertize (substring str (match-beginning 1) (match-end 1))
  								 'face 'acc-rg-path-face))
  				   (propertize (substring str (match-beginning 2) (match-end 2))
  							   'face 'acc-rg-filename-face)))
  		  (propertize " " 'display '(space . (:align-to 80)))

		  (let ((lisp-str (substring str (match-beginning 4) (match-end 4)) ))
			,(if (and buf major-mode)
				 `(with-current-buffer ,buf
					(erase-buffer)
					(insert lisp-str)
					(font-lock-default-fontify-region (point-min)
													  (point-max)
													  nil)
					(buffer-string))
			   `lisp-str)))
  	   str)))

(ivy-set-display-transformer 'counsel-ag (acc-counsel-rg-transformer nil))
(ivy-set-display-transformer 'counsel-rg (acc-counsel-rg-transformer nil))

(defsubst acc--thing-at-point ()
  (if (ignore-errors (string-match "[a-zA-Z0-9]" (char-to-string (char-after))))
	  (ivy-thing-at-point)
	nil))

;;;###autoload
(defun acc-counsel-projectile-rg-handler (arg)
  (interactive "P")

  (let* ((thing-at-point (acc--thing-at-point))
		 (ivy-display-style 'fancy)
		 (filetype-arg (pcase major-mode
						 ('emacs-lisp-mode "-telisp")
						 ('c-mode "-tcpp -tcmake")
						 ('c++-mode "-tcpp -tcmake")
						 ('org-mode "-torg -tmarkdown -trst -ttxt -treadme")
						 ('markdown-mode "-torg -tmarkdown -trst -ttxt -treadme")
						 ('python-mode "-tpy")
						 ('cmake-mode "-tcmake")
						 ('vhdl-mode "-tvhdl")
						 ('haskell-mode "-thaskell")
						 (_ nil)))
		 (coloring-enabled (-contains? (list 'emacs-lisp-mode
		  									 'cmake-mode
		  									 'haskell-mode)
		  							   major-mode
		  							   ))
		 (temp-buffer (get-buffer-create "*rg-color-buffer*"))
		 (major-mode-outer major-mode)
		 (ivy--display-transformers-list
		  (plist-put ivy--display-transformers-list
					 'counsel-ag
					 (if coloring-enabled
						 (acc-counsel-rg-transformer temp-buffer major-mode)
					   (acc-counsel-rg-transformer)))))
	(with-current-buffer temp-buffer
	  (read-only-mode -1)
	  (font-lock-mode)
	  (delay-mode-hooks (funcall major-mode-outer))
	  (font-lock-default-function major-mode-outer))
	(if (not arg)
		(progn
		  ;; -----------------------------------------------
		  ;; default:  query for search string, optimize for speed
		  (acc-counsel-projectile-rg-query thing-at-point filetype-arg))

	  ;; ---------------------------------------------------
	  ;; prefix arg:  full interactive search, optimize for accuracy
	  (acc-counsel-projectile-rg thing-at-point))))

;;;###autoload
(defun acc-counsel-projectile-rg (&optional initial-input)
  "Search for string in project files."
  (interactive)
  (let* ((add-paths-alist (pjson-get-search-paths))
		 (add-paths-str " ")
		 (options (concat " -j 6 -- " add-paths-str))
		 )
	(while add-paths-alist
	  (setq add-paths-str (concat add-paths-str " " (pop add-paths-alist))))

	(if (and (eq projectile-require-project-root 'prompt)
			 (not (projectile-project-p)))
		(counsel-projectile-rg-action-switch-project)

      (let* ((ivy--actions-list (copy-sequence ivy--actions-list))
			 (path (mapconcat 'shell-quote-argument
							  (or (projectile-normalise-paths
								   (car (projectile-parse-dirconfig-file)))
								  '("."))
							  " "))
			 (ignored (mapconcat (lambda (i)
								   (concat "--glob !" (shell-quote-argument i)))
								 (append
								  (projectile--globally-ignored-file-suffixes-glob)
								  (projectile-ignored-files-rel)
								  (projectile-ignored-directories-rel))
								 " "))
			 (counsel-rg-base-command (format
									   (string-trim-right counsel-rg-base-command " \\.")
									   (concat ignored " %s " path))))
		(ivy-add-actions 'counsel-ag
						 counsel-projectile-rg-extra-actions)
		(counsel-rg initial-input
					(projectile-project-root)
					options
					(projectile-prepend-project-name
					 (concat (car (split-string counsel-rg-base-command))
							 (if (null add-paths-alist) ": " "*: "))))))))

;;;###autoload
(defun acc-counsel-projectile-rg-query (&optional initial-input extra-rg-args)
  "As counsel-rg, but the query-string is asked for beforehand
  and static."
  (interactive)
  (when (not (projectile-project-p))
	(counsel-projectile-rg-action-switch-project))

  (let* ((project-name (or (pjson-current-project)
						   (ignore-errors (f-filename (projectile-project-root)))
						   ""))
		 (default-directory (or (projectile-project-root)
								default-directory))
		 (prompt (if (s-blank? project-name)
					 (concat "rg> ")
				   (concat "rg [" (propertize project-name 'face '(:foreground "#ac443f")) "]> ")))
		 (search-string (if initial-input initial-input
						  (read-string prompt)))
		 (curbuf (current-buffer))
		 (curpoint (point))
		 (path (mapconcat 'shell-quote-argument
		 				  (or (projectile-normalise-paths
		 					   (car (projectile-parse-dirconfig-file)))
		 					  '("."))
		 				  " "))
		 (ignored (mapconcat (lambda (i)
							   (concat "--glob !" (shell-quote-argument i)))
							 (append
		 					  (projectile--globally-ignored-file-suffixes-glob)
		 					  (projectile-ignored-files-rel)
		 					  (projectile-ignored-directories-rel)
							  )
		 					 " "))
		 (rg-command (concat (format
										   (string-trim-right counsel-rg-base-command  " \\.")
		 								   (concat ignored " %s " path))
										  " " extra-rg-args)))

	(setq counsel-ag-command rg-command)

	(when (and search-string
			   (not (s-blank? search-string)))
      (ivy-read prompt
				#'(lambda (&rest _) (counsel-ag-function search-string))
				:initial-input ""
				:dynamic-collection nil
				:keymap counsel-ag-map
				:history 'counsel-git-grep-history
				:action #'counsel-git-grep-action
				:unwind (lambda ()
						  (counsel-delete-process)
                          (swiper--cleanup)
						  (switch-to-buffer curbuf)
						  (goto-char curpoint)
						  )
				:caller 'counsel-ag))))

;;;###autoload
(defun acc-counsel-projectile-rg-subdir-downwards-handler (arg)
  (interactive "P")

  (let* ((thing-at-point (acc--thing-at-point))
		 (ivy-display-style 'fancy)
		 (filetype-arg (pcase major-mode
						 ('emacs-lisp-mode "-telisp")
						 ('c-mode "-tcpp -tcmake")
						 ('c++-mode "-tcpp -tcmake")
						 ('org-mode "-torg -tmarkdown -trst -ttxt -treadme")
						 ('markdown-mode "-torg -tmarkdown -trst -ttxt -treadme")
						 ('python-mode "-tpy")
						 ('cmake-mode "-tcmake")
						 ('vhdl-mode "-tvhdl")
						 ('haskell-mode "-thaskell")
						 (_ nil)))
		 (coloring-enabled (-contains? (list 'emacs-lisp-mode
											 'cmake-mode
											 'haskell-mode)
									   major-mode
									   ))
		 (temp-buffer (get-buffer-create "*rg-color-buffer*"))
		 (major-mode-outer major-mode)
		 (ivy--display-transformers-list
		  (plist-put ivy--display-transformers-list
					 'counsel-rg
					 (if coloring-enabled
						 (acc-counsel-rg-transformer temp-buffer major-mode)
					   (acc-counsel-rg-transformer)))))
	(with-current-buffer temp-buffer
	  (read-only-mode -1)
	  (font-lock-mode)
	  (delay-mode-hooks (funcall major-mode-outer))
	  (font-lock-default-function major-mode-outer))
	(if (not arg)
		;; -----------------------------------------------
		;; default:  query for search string, optimize for speed
		(acc-counsel-from-subdir-downwards-rg-query thing-at-point filetype-arg)

	  ;; ---------------------------------------------------
	  ;; prefix arg:  full interactive search, optimize for accuracy
	  (acc-counsel-from-subdir-downwards-rg thing-at-point))))

;;;###autoload
(defun acc-counsel-from-subdir-downwards-rg-query (&optional initial-input extra-rg-args)
  "Search for string in files from current subdirectory
  downward."
  (interactive)

  (let* ((prompt (concat "rg [" (propertize default-directory 'face '(:foreground "#ac443f")) "]> "))
		 (search-string (if initial-input initial-input
						  (read-string prompt)))
		 (curbuf (current-buffer))
		 (curpoint (point))
		 (rg-command (concat
					  (string-trim-right counsel-rg-base-command)
					  ;; " ."
					  " " extra-rg-args)))

	(setq counsel-ag-command rg-command)

	(ivy-read prompt
			  #'(lambda (&rest _) (counsel-ag-function search-string))
			  :initial-input ""
			  :dynamic-collection nil
			  :keymap counsel-ag-map
			  :history 'counsel-git-grep-history
			  :action #'counsel-git-grep-action
			  :unwind (lambda ()
						(counsel-delete-process)
						(swiper--cleanup)
						(switch-to-buffer curbuf)
						(goto-char curpoint)
						)
			  :caller 'counsel-ag))
  )

;;;###autoload
(defun acc-counsel-from-subdir-downwards-rg (&optional initial-input extra-rg-args)
  "Search for string in files from current subdirectory
  downward."
  (interactive)

  (let* ((prompt (concat "rg [" (propertize default-directory 'face '(:foreground "#ac443f")) "]> "))
		 (options (concat " -j 6 " ))
		 (rg-command (concat
					  (string-trim-right counsel-rg-base-command)
					  ;; " ."
					  " " extra-rg-args)))

	(let* (ivy--actions-list (copy-sequence ivy--actions-list))

	  (ivy-add-actions 'counsel-ag
					   counsel-projectile-rg-extra-actions)
	  (counsel-rg initial-input
				  default-directory
				  options
				  prompt))))

;;;###autoload
(defun acc-counsel-projectile-find-file ()
  "Jump to a file in the current project. Files are sorted by
recent buffer activity. Additional pjson search paths are
included."
  (interactive)
  (let* ((additional-paths (pjson-get-search-paths))
		 (projectile-known-projects (remove nil additional-paths))
		 (additional-paths-indicator (if (null additional-paths) ": " "*: " )))
	(ivy-read (projectile-prepend-project-name (concat "Find file" additional-paths-indicator))
			  (nconc (projectile-current-project-files) (projectile-all-project-files))
			  :matcher counsel-projectile-find-file-matcher
			  :require-match t
			  :sort nil
			  :action counsel-projectile-find-file-action
			  :caller 'acc-counsel-projectile-find-file)))

;;;###autoload
(defun acc-ido-switch-buffer (arg)
  "Switch buffer using modebar completion."
  (interactive "P")
  ;; -- default:  search in ivy-buffer-list
  (if (not arg)
	  (let* ((selection (completing-read "buffer> " (ivy--buffer-list "" t) nil t))
			 (virtual (assoc selection ivy--virtual-buffers))
			 buffer)
		(when selection
		  (cond ((setq buffer (get-buffer selection))
				 (switch-to-buffer buffer nil 'force-same-window))
				(virtual
				 (find-file (cdr virtual)))
				((file-exists-p selection)
				 (find-file selection)))))
	;; prefix argument:  search for *.el and dirs in emacs directory
	(let* ((root (concat dir_emacs "modules/"))
		   (cmd-files "fd '.el$'")
		   (cmd-dirs "fd . -t d")
		   (cmd (concat "cd " root "; " cmd-files "; " cmd-dirs))
		   (cands (split-string  (with-output-to-string
								   (with-current-buffer standard-output
									 (shell-command cmd t))) "[\n]" t))
		   (selection (completing-read "config> " cands nil t)))
	  (when selection
		(let* ((path (concat root selection))
			   (is-dir (f-directory? path))
			   (is-file (f-file? path)))
		  (cond ((f-directory? path)
				 (dired path))
				((f-file? path)
				 (find-file path))))))))

;;;###autoload
(defun acc-ido-projectile-find-file ()
  "Jump to a file in the current project.
Include additional pjson search paths"
  ;; NOTE: fn-counsel-projectile-find-file
  (interactive)
  (let* ((additional-paths (pjson-get-search-paths))
		 (projectile-known-projects (remove nil additional-paths))
		 (selection (completing-read (concat (projectile-project-name) (if (null additional-paths) "> " "*> "))
									 (nconc (projectile-current-project-files) (projectile-all-project-files))
									 nil t
									 )))
	(when selection						; TODO: factor out
	  (let* ((path (concat (projectile-project-root) selection))
			 (is-dir (f-directory? path))
			 (is-file (f-file? path)))
		(cond ((f-directory? path)
			   (dired path))
			  ((f-file? path)
			   (find-file path)))))))

;;;###autoload
(defun acc-ido-find-file-in-subdir-downwards ()
  "Jump to a file in the current directory downwards."
  (interactive)
  (let* ((cmd-files "fd .")
		 (cmd-dirs "fd . -t d")
		 (cmd (concat cmd-files "; " cmd-dirs))
		 (cands (split-string	(with-output-to-string
								  (with-current-buffer standard-output
									(shell-command cmd t))) "[\n]" t))
		 (selection (completing-read (concat default-directory "> ") cands nil t)))
	(when selection						; TODO: factor out
	  (let* ((path (concat default-directory selection))
			 (is-dir (f-directory? path))
			 (is-file (f-file? path)))
		(cond ((f-directory? path)
			   (dired path))
			  ((f-file? path)
			   (if (setq buffer (get-buffer (f-filename path)))
				   (switch-to-buffer buffer)
				 (find-file path))))))))

;;;###autoload
(defun acc-M-x ()
  (interactive)
  (let* ((cmd (read-command "command> ")))
	;; (message "executing %s" cmd)
	(when (commandp cmd)
	  (cond ((bound-and-true-p amx-initialized)
			 (amx-rank cmd))
			((bound-and-true-p smex-initialized-p)
			 (smex-rank cmd)))
	  (command-execute cmd 'record))))

;;;###autoload
(defun acc-fasd-find (file_or_dir &optional query)
  "Open best match without fuzzying anything.
file_or_dir > 0, file					;
file_or_dir nil, dir"
  (interactive)
  (if (not (executable-find "fasd"))
      (error "Fasd executable cannot be found.  It is required by `fasd.el'.  Cannot use `fasd-find-file'")
    (unless query
	  (setq query (read-from-minibuffer "> "))
	  )
    (let* ((prompt "Fasd query: ")
           (results
            (split-string
             (shell-command-to-string
              (concat "fasd -lR "
                      (if (eq file_or_dir 'file)
                          "-f " "-d ")
                      query))
             "\n" t)
			))
      (if (eq file_or_dir 'file)
          (find-file (car results))
		(dired (car results))))))

;;;###autoload
(defun acc-projectile-replace ()
  "Replace string in current projectile project, if we are in
one, or subdirectories if we're not. Queries the user for the
string to replace (default: thing-at-point), and for the string
(regex?) to replace it with."
  (interactive)
  (let* ((old-text (read-string
					(projectile-prepend-project-name "Replace: ")
					(projectile-symbol-or-selection-at-point)))
		 (new-text (read-string
					(projectile-prepend-project-name
					 (format "Replace %s with: " old-text))))
		 (files (if (projectile-project-p)
					(let ((default-directory (projectile-project-root)))
					  (-map (lambda (f) (if (f-relative? f)
									   (file-truename f) f))
							(projectile-current-project-files)))
				  (directory-files-recursively default-directory ""))))
	(when (multifile-initialize-replace old-text new-text files 'default)
	  (multifile-continue))))

;; TODO: implement fn-find-project
;; (require 'fuzzy-match)
;; (defun fn-find-project (&optional query)
;;   (interactive)
;;   (unless query
;;     (setq query (read-from-minibuffer "> ")))
;;   (let ((matches (FM-all-fuzzy-matches query projectile-known-projects)))
;;     (prin1 matches)
;;     )
;;   )

;;;; code-repl toggle

;; -> access/repl

;; ---------------------------------------------------------
;; repl triplets:  (MINOR-MODE, BUFFER-NAME, RUN-COMMAND)
;; buffer name can be a string or function to return a string
;; run-command can be a string, symbol, or function to return a symbol
(setq fn-major-mode-repl-alist '( ( "lisp-mode" "*slime-repl sbcl*" "slime" )
								  ( "python-mode" "*Python*" "+access-repl-run-python" )
								  ( "haskell-mode"
									(lambda () (if (fboundp #'intero-repl-buffer)
											  (buffer-name (intero-repl-buffer nil))
											""))
									"intero-repl" )
								  ( "emacs-lisp-mode" "*ielm*" "ielm" )))

;; ---------------------------------------------------------
;;;###autoload
(defun +access-repl-run-python ()
  (interactive)
  (let ((interpreter-root (if (boundp 'interpreter-root) interpreter-root nil))
		(buffer (python-shell-make-comint
				 (python-shell-calculate-command)
				 (python-shell-get-process-name nil) nil)))
	(pop-to-buffer-same-window buffer)
	(when interpreter-root
	  (python-shell-send-string (concat "cd " interpreter-root))
	  (goto-char (point-max))
	  )))

(defun +access-repl-run-ielm-here (&optional buf-name)
  (interactive)
  (let (old-point
        (buf-name (or buf-name "*ielm*")))
    (unless (comint-check-proc buf-name)
      (with-current-buffer (get-buffer-create buf-name)
        (unless (zerop (buffer-size)) (setq old-point (point)))
        (inferior-emacs-lisp-mode)))
    (pop-to-buffer-same-window buf-name)
    (when old-point (push-mark old-point)))
  )

;;;###autoload
(defun +access-repl-triplet-from-major-mode (mode)
  "Return repl triplet for major MODE if MODE in repl-alist,
return python triplet otherwise."
  (let ((r (assoc (symbol-name mode) fn-major-mode-repl-alist)))
	;; if `major-mode' not in list, fall back to python triplet
	(if (null r)
		(assoc "python-mode" fn-major-mode-repl-alist)
	  r)))

;;;###autoload
(defun +access-repl-triplet-buffer-name (triplet)
  "Return `buffer-name' if it is a string, otherwise execute it."
  (let ((n (cadr triplet)))
	(if (stringp n) n (funcall n))))

;;;###autoload
(defun +access-repl-triplet-run-command (triplet)
  "Return `run-command` as a funcall-able symbol or closure."
  (let ((run-thing (caddr triplet)))
	(if (stringp run-thing) (intern run-thing) run-thing)))

;;;###autoload
(defun +access-repl-run-defun (triplet)
  (intern (caddr triplet)))

;; ---------------------------------------------------------
;;;###autoload
(defun +access-repl-inside-p ()
  "Return T if inside a window showing a repl buffer, nil
  otherwise."
  (let ((bufname (buffer-name)))
	(dolist (triplet fn-major-mode-repl-alist)
	  (when (string= bufname (+access-repl-triplet-buffer-name triplet))
		(return t)))))

;;;###autoload
(defun +access-repl-matching-visible ()
  "Return the window in the current frame that contains a repl
buffer that matches the current major-mode, if it exists, or nil
otherwise."
  (let ((target-buffer-name (+access-repl-triplet-buffer-name
							 (+access-repl-triplet-from-major-mode major-mode))))
	(cl-loop for win in (window-list) do
			 (when (string= (buffer-name (window-buffer win))
							target-buffer-name)
			   (return win)))))

;;;###autoload
(defun +access-repl-create (new-repl-run-command)
  (funcall new-repl-run-command)
  (evil-force-normal-state))

;;;###autoload
(defun +access-repl-close (&optional win)
  "Closes the repl shown in WIN or CURRENT-WINDOW."
  (let ((w (if win win (selected-window))))
	;; window-parameter
	(cl-loop for frame-window in (window-list)
			 do (when (eq (window-parameter frame-window 'associated-repl)
						  w)
				  (set-window-parameter frame-window 'associated-repl nil)))
	;; frame-parameter
	(when (eq (frame-parameter (selected-frame) 'associated-repl) w)
	  (set-frame-parameter (selected-frame) 'associated-repl nil))
	(delete-window w)))

;; ---------------------------------------------------------
;;;###autoload
(defun +access-repl-window-close ()
  "Closes the repl belonging to the current window."
  (let ((repl-window (window-parameter (selected-window) 'associated-repl)))
	(when repl-window
	  (delete-window repl-window)
	  (set-window-parameter (selected-window) 'associated-repl nil))))

;;;###autoload
(defvar +access-repl-window-size 0.2
  "The initial size of repl windows associated with a window,
  relative to the size of it. The configuration for repl windows
  associated with the frame is done externally e.g. using popwin
  or purpose.")

;;;###autoload
(defun +access-repl-window-create (new-repl-run-command)
  (popwin:without-special-displaying
   (without-purpose
	 (let ((win (selected-window))
		   (size (round (* (window-height) (- 1 +access-repl-window-size)))))
	   (split-window-below-and-focus size)
	   (+access-repl-create new-repl-run-command)
	   (set-window-parameter win 'associated-repl (selected-window))))))

;;;###autoload
(defun +access-repl-window-replace (new-repl-run-command)
  (let ((repl-window (window-parameter (selected-window) 'associated-repl)))
	(when (window-live-p repl-window)
	  (popwin:without-special-displaying
	   (without-purpose
		 (select-window repl-window t)
		 (+access-repl-create new-repl-run-command))))))

;; ---------------------------------------------------------
;;;###autoload
(defun +access-repl-frame-close ()
  "Closes the repl belonging to the current frame."
  (let ((repl-window (frame-parameter (selected-frame) 'associated-repl)))
	(when repl-window
	  (delete-window repl-window)
	  (set-frame-parameter (selected-frame) 'associated-repl nil))))

;;;###autoload
(defun +access-repl-frame-create (new-repl-run-command)
  (without-purpose
	(+access-repl-create new-repl-run-command)
	(set-frame-parameter (selected-frame) 'associated-repl (selected-window))))

;; ---------------------------------------------------------
;;;###autoload
(defun fn-toggle-repl-window (arg)
  "Displays repl fitting to the current major mode below the
  current window or, if called with prefix argument, at the
  bottom of the current frame. Hides the repl window if it is
  already shown, irregardless of where it is shown. Replaces the
  repl shown at the bottom of the frame if a different one is
  already shown there."
  (interactive "p")
  (if (and (eq arg 1) (+access-repl-inside-p))
	  (+access-repl-close)
	(let* ((matching-repl-win (+access-repl-matching-visible))
		   (matching-repl-visible-p (when matching-repl-win t))
		   (repl-run-command (+access-repl-triplet-run-command (+access-repl-triplet-from-major-mode major-mode)))
		   (window-repl (let ((win (window-parameter (selected-window) 'associated-repl)))
						  (if (window-live-p win) win nil)))
		   (window-repl-p (when window-repl t))
		   (frame-repl (let ((win (frame-parameter (selected-frame) 'associated-repl)))
						 (if (window-live-p win) win nil)))
		   (frame-repl-p (when frame-repl t)))

	  (pcase `(,arg ,matching-repl-visible-p ,window-repl-p ,frame-repl-p)
		(`(,_ t ,_ ,_) 					; -- repl visible -> close
		 ;; (message "repl close")
		 (+access-repl-close matching-repl-win))
		(`(1 nil t ,_ )					; -- no prefix arg, window has repl
		 ;; (message "window replace")
		 (+access-repl-window-replace repl-run-command))
		(`(1 nil nil ,_)				; -- no prefix arg, window has no repl
		 ;; (message "window create")
		 (+access-repl-window-create repl-run-command))
		(`(4 nil ,_ nil)  				; -- prefix arg, frame has no repl
		 ;; (message "frame create")
		 (+access-repl-frame-create repl-run-command))
		(`(4 nil ,_ t )					; -- prefix arg
		 ;; (message "frame close")
		 (+access-repl-frame-close)))
	  )))

(run-with-timer 5 nil (lambda ()
						(spacemacs/set-leader-keys "#" 'fn-toggle-repl-window)))

;;;; eshell toggle

;; TODO:  side-windows
(setq window-sides-vertical t)
;; (display-buffer-in-side-window "*scratch*" '((side . bottom)))
;; https://www.gnu.org/software/emacs/draft/manual/html_node/elisp/Frame-Layouts-with-Side-Windows.html#Frame-Layouts-with-Side-Windows
;; https://www.gnu.org/software/emacs/draft/manual/html_node/elisp/Displaying-Buffers-in-Side-Windows.html#Displaying-Buffers-in-Side-Windows

;; ---------------------------------------------------------
;; A list containing (window eshell-num) tuples determining which window is
;; associated with which eshell.
(setq +access-eshell-window-repl-assoc '())

;;;###autoload
(defun +access-eshell-bufname-from-num (num)
  (format "%s<%d>" eshell-buffer-name num))

;;;###autoload
(defun +access-eshell-num-from-bufname (bufname)
  (when (string-match (concat (regexp-quote eshell-buffer-name) "<\\([0-9]*\\)>") bufname)
	(string-to-number (match-string 1 bufname))))

;;;###autoload
(defun +access-eshell-win-from-num (num-arg)
  (cl-loop for (win num) in +access-eshell-window-repl-assoc
		   do (when (eq num-arg num) (return win))))

;;;###autoload
(defun +access-eshell-assoc-win-from-eshell-win (&optional win)
  (let* ((w (if win win (selected-window)))
		 (eshell-num (+access-eshell-num-from-bufname (buffer-name (window-buffer w)))))
	(when eshell-num
	  (+access-eshell-win-from-num eshell-num))))

;;;###autoload
(defun +access-eshell-get-associated-eshell (&optional win)
  (let* ((w (if win win (selected-window)))
		 (eshell-num (cadr (assoc w +access-eshell-window-repl-assoc))))
	(if eshell-num
		(get-buffer (+access-eshell-bufname-from-num eshell-num))
	  nil)))

;;;###autoload
(defun +access-eshell-associate (win num)
  "Associates eshellbuf NUM with WIN."
  (if +access-eshell-window-repl-assoc
	  (add-to-list '+access-eshell-window-repl-assoc `(,win ,num))
	(setf +access-eshell-window-repl-assoc `((,win ,num)))))

;;;###autoload
(cl-defun +access-eshell-disassociate (&key win eshellbuf)
  "Disassociates eshellbuf from win, (NOTE: and kills eshellbuf?).
If WIN is nil, use the current window. If eshellbuf is not given,
determine it if possible."
  (let* ((win (if win win (selected-window)))
		 (eshellbuf (if eshellbuf eshellbuf
					  (+access-eshell-get-associated-eshell win)))))
  (setf +access-eshell-window-repl-assoc
		(remove (assoc win +access-eshell-window-repl-assoc) +access-eshell-window-repl-assoc))
  ;; (when (buffer-live-p eshellbuf)
  ;; 	(message "buffer live, deleting it...")
  ;; 	(kill-buffer eshellbuf))
  )

;;;###autoload
(defun +access-eshell--get-free-num ()
  "Find a free eshell number."
  (if (null +access-eshell-window-repl-assoc)
	  1
	(let* ((sorted-taken-nums
			(sort (cl-loop for (_ num) in +access-eshell-window-repl-assoc
						   collect num) '<))
		   (free-inbetween (seq-difference
							(number-sequence 1 (seq-max sorted-taken-nums))
							sorted-taken-nums)))
	  (if (null free-inbetween)
		  (+ 1 (seq-max sorted-taken-nums))
		(car free-inbetween)))))

;;;###autoload
(defun +access-eshell-buf-get-create (&optional win)
  "Get eshell buffer for WIN, create new one if necessary."
  (let ((win (if win win (selected-window)))
		(existing-eshell-buf (+access-eshell-get-associated-eshell win)))
	(if existing-eshell-buf existing-eshell-buf
	  (let* ((new-eshell-num (+access-eshell--get-free-num))
			 (new-eshell-buf
			  (get-buffer-create (+access-eshell-bufname-from-num
								  new-eshell-num))))
		(+access-eshell-associate win new-eshell-num)
		new-eshell-buf))))

;; ---------------------------------------------------------

(defvar +access-eshell-window-size 0.2
  "The initial size of shell windows associated with a window,
  relative to the size of it. The configuration for eshell
  windows associated with the frame is done externally e.g. using
  popwin or purpose.")

;;;###autoload
(defun +access-eshell-open ()
  "Opens an eshell for the current window."
  (let ((eshell-buf (+access-eshell-buf-get-create))
		(size (round (* (window-height) (- 1  +access-eshell-window-size))))
		(current-dir default-directory))
	(without-purpose
	  (split-window-below-and-focus size)
	  (switch-to-buffer eshell-buf)
	  ;; change/modify buf
	  (unless (derived-mode-p 'eshell-mode)
		(eshell-mode)
		(evil-force-normal-state))
	  ;; (+eshell-clear)
	  (when (not (string= current-dir default-directory))
		(cd current-dir)
		(eshell-send-input)))))

;;;###autoload
(defun +access-eshell-close ()
  "Closes the eshell for the current window if it exists and is
  open, but keeps it available in the background."
  (let* ((eshell-buf (+access-eshell-get-associated-eshell)))
	(when eshell-buf
	  (delete-window (get-buffer-window eshell-buf)))))

;;;###autoload
(defun +access-eshell--on-delete-window (&optional window)
  "If a window is being closed that has a shell window associated
with it, disassociate it."
  (let ((window (if window window (selected-window))))
	(when (not (eq 'eshell-mode (with-current-buffer (window-buffer window)
								  major-mode)))
	  (+access-eshell-disassociate :win window))))

(advice-add 'delete-window :before #'+access-eshell--on-delete-window)

;; ---------------------------------------------------------

;;;###autoload
(defun +access-eshell-open-create-frame-eshell ()
  "Open or create an eshell session associated with the current
  frame."
  (let ((eshell-buf (frame-parameter (selected-frame) 'associated-eshell)))
	(when (or (not eshell-buf)
			  (not (buffer-live-p eshell-buf)))
	  (with-current-buffer
		  (setf eshell-buf (get-buffer-create (format "*eshell-frame*<%s>" (selected-frame))))
		(unless (derived-mode-p 'eshell-mode)
		  ;; NOTE: less context-sensitive than what happens in +access-eshell-open
		  (eshell-mode)
		  (evil-force-normal-state)))
	  (set-frame-parameter (selected-frame) 'associated-eshell eshell-buf))
	;; (pop-to-buffer eshell-buf)
	(display-buffer-in-side-window eshell-buf '('side bottom))
	))

;;;###autoload
(defun +access-eshell-close-frame-eshell ()
  "Close the eshell session window associated with the current
  frame."
  (let ((eshell-buf (frame-parameter (selected-frame) 'associated-eshell)))
	(if (buffer-live-p eshell-buf)
		(if (get-buffer-window eshell-buf)
			(delete-window (get-buffer-window eshell-buf)))
	  (set-frame-parameter (selected-frame) 'associated-eshell nil))))

;; ---------------------------------------------------------

;;;###autoload
(defun +access-eshell-inside-assoc-eshell-window-p ()
  "Are we inside an eshell window that is associated with a window?"
  (let* ((eshell-num (+access-eshell-num-from-bufname (buffer-name)))
		 (assoc-buffer (when eshell-num (+access-eshell-win-from-num eshell-num))))
	(if assoc-buffer t nil)))

;;;###autoload
(defun +access-eshell-window-eshell-open-p ()
  "Does the current window have its eshell window open, or are we
in an eshell window that is associated with a window?"
  (let* ((eshell-buf (+access-eshell-get-associated-eshell)))
	(if eshell-buf (if (get-buffer-window eshell-buf) t nil))))

;;;###autoload
(defun +access-eshell-frame-eshell-open-p ()
  ;; NOTE: duplicate in +access-eshell-close-frame-eshell
  (let ((eshell-buf (frame-parameter (selected-frame) 'associated-eshell)))
	(when (buffer-live-p eshell-buf)
	  (when (get-buffer-window eshell-buf) t))))

;; ---------------------------------------------------------

;;;###autoload
(defun +access-eshell-toggle (arg)
  "Toggles an eshell session window that stays associated with it
for the lifetime of the current window."
  (interactive "p")
  ;; inside an eshell session window already? Just close/hide it.
  (let ((outside-open-p (+access-eshell-window-eshell-open-p))
		(inside-p (+access-eshell-inside-assoc-eshell-window-p)))
	(pcase `(,arg ,outside-open-p ,inside-p)
	  (`(1 ,_ t) (delete-window))
	  (`(1 t ,_) (+access-eshell-close))
	  (`(1 nil nil) (+access-eshell-open))
	  (`(4 ,_ ,_) (if (+access-eshell-frame-eshell-open-p)
					  (+access-eshell-close-frame-eshell)
					(+access-eshell-open-create-frame-eshell))))))

(run-with-timer 5 nil (lambda ()
						(spacemacs/set-leader-keys "'" '+access-eshell-toggle)))

;; ---------------------------------------------------------



(message "... done!")
(provide 'ui/access/access)
