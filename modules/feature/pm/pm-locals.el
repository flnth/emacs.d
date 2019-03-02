;;; modules/feature/pm/pm-locals.el             -*- lexical-binding: t; -*-
;; Functions for dealing with directory-local variables.

(require 'cl-generic)

(defvar pm-locals--main-stash-vars (list
									'project-name
									'project-meta-dir
									'projectile-project-root
									'current-target
									'targets
									)
  "List of base-variables every pm .dir-locals.el has in category
  nil." )

(cl-defun pm-locals--main-stash-create (root &optional name meta-root)
  (let ((stash
		 (list (cons 'nil (seq-mapcat
						   (lambda (x) (list (cons x nil)))
						   pm-locals--main-stash-vars)))))
	(setf (+alist-get '(nil projectile-project-root) stash) root)
	(setf (+alist-get '(nil project-name) stash) name)
	(setf (+alist-get '(nil project-meta-dir) stash) meta-root)
	stash))

(defun pm-locals--is-main-stash (stash)
  (pm-list-conforms-to
   `((nil . (,@pm-locals--main-stash-vars))) stash))

(defvar pm-locals--main-vars-template
  `((nil . (,@pm-locals--main-stash-vars)))
  "Template that identifies a top-level pm stash.")

;; ---------------------------------------------------------

(defvar pm-locals-find-recursion-depth 2
  "The default depth stashes are being searched for when looking
downwards the directory hierarchy. Used for initial construction
of caches, or stash-buffer creation via pm-locals-view.")

(cl-defun pm-locals--collect (&optional cache root)
  "Collects all pm-local stashes below ROOT, if ROOT is given,
and returns their content as an association list with their path
as key. If ROOT is not given, the pm project-root is searched for
given the current directory. If CACHE is nil, the content of
.dir-locals.el files is shown; if CACHES is t, the content of
stashes is shown."
  (condition-case err
	  (let* ((root (if root (if (file-exists-p root) root
							  (error "Root does not exist: %s" root))
					 (if cache
						 (s-chop-suffix ".dir-locals.el"
										(pm-locals-find-main-file default-directory))
					   (pm-locals-find-main-file default-directory))))
			 (sub-dirlocals (if cache
								(pm-locals-find-caches-downwards root)
							  (pm-locals-find-files-downwards root nil
															  pm-locals-find-recursion-depth)))
			 (stash-paths (cons root (remove root sub-dirlocals)))
			 (content (-zip stash-paths
							(if cache
								(->> stash-paths
									 (-map (lambda (c) (pm-locals-read-cache c))))
							  (->> stash-paths
								   (-map (lambda (f) (pm-locals-read-file f))))))))

		content)))

(cl-defun pm-locals--view (stashes)
  "Creates a buffer in `emacs-lisp-mode' for viewing STASHES in a
pretty-printed but still parseable form. STASHES should be an
association list with paths to the corresponding stashes as keys.
STASHES are visually grouped together with their main stash on
top and sub-stashes below. Returns the buffer with the content
printed."

  ;; (map-keys stashes)

  ;; find those paths which are a prefix of another.
  ;; 


  (with-current-buffer "test"
	(erase-buffer)
	(delay-mode-hooks (emacs-lisp-mode))
	(dolist (el content)
	  (insert (format "(\"%s\"" (car el)))
	  (newline)
	  (insert (pm--dir-locals-to-string (cdr el)))
	  (insert ")")
	  (newline)
	  )
	(indent-region (point-min) (point-max) nil)

	)


  )

;; TODO:  a minor-mode for "stash-buffers"
;;  - next/previous stash
;;  - (kbd "C-c c") to apply (intelligently)
;;  - (kbd "gr") to refresh
;;  - when showing files:  show differences between file/cache
;;  -

(cl-defun pm-locals-create (root &optional
								 (initial (list (cons 'nil 'nil)))
								 overwrite)
  "Create new stash in ROOT: file and cache. If either exists,
  and OVERWRITE is t, overwrite either, otherwise merge."
  (when (not (f-exists?
			  root))
	(signal 'file-missing (list 'f-exists? root)))
  (let ((root (f-slash root))
		(ex-cache (ignore-errors (pm-locals-read-file root)))
		(ex-file (ignore-errors (pm-locals-read-cache root)))
		(new-stash initial)
		(root-sym (intern (s-collapse-whitespace root))))
	(setf (+alist-get '(nil projectile-project-root) new-stash)
		  root)

	(when (and overwrite ex-cache)
	  (setf new-stash (pm-list-merge :from ex-cache :to new-stash)))
	(when (and overwrite ex-file)
	  (setf new-stash (pm-list-merge :from ex-file :to new-stash)))

	(pm-locals-write-file root new-stash nil)
	(pm-locals-write-cache root new-stash)
	t))

(cl-defun pm-locals-find-file (&optional (from-path default-directory) template)
  "Find FROM-PATH, including itself, a dir-locals stash, file or
cache satisfying TEMPLATE, if given. If no TEMPLATE is given,
return the first stash that has been found."
  (let ((default-directory from-path)
		(do-search t)
		path)
	(while do-search
	  (setf path (locate-dominating-file default-directory ".dir-locals.el"))
	  (if path
		  ;; verify if template set
		  (if template
			  (if (pm-list-conforms-to template
									   (pm-locals-read-file path))
				  ;; found, stop
				  (setf do-search nil)
				;; continue searching
				(setf default-directory (f-parent path)))
			;; no verification necessary, stop
			(setf do-search nil))
		;; at system root, stop
		(setf do-search nil)))
	(if path
		(setf path (concat (f-slash (expand-file-name path))
						   ".dir-locals.el"))
	  (signal 'file-missing (list "No .dir-locals.el conforming to template found."
								  template
								  from-path)))))

(cl-defun pm-locals-find-main-file (&optional (from-path default-directory))
  "Find the main stash file up from FROM-PATH, including itself."
  (let* ((cache (ignore-errors
				  (pm-locals-find-cache default-directory pm-locals--main-vars-template)))
		 (file  (when (not cache)
				  (ignore-errors
					(concat (f-slash (pm-locals-find-file default-directory pm-locals--main-vars-template)))))))
	(concat (f-slash
			 (cond (cache cache)
				   (file file)
				   (t (signal 'main-stash-missing (list (format "Searched upwards from: %s" from-path))))))
			".dir-locals.el")))

(cl-defun pm-locals-find-files-downwards (&optional (from-path default-directory) template depth)
  "Like pm-locals-find-file, but searches downwards the
  directory-hierarchy instead of upwards. If DEPTH is nil recurse
  indefinitely, if it is an integer N, recurse N levels into
  from-path."
  (let ((default-directory from-path)
		(candidates (+locate-dominating-file-downwards default-directory
													   ".dir-locals.el"
													   (if depth depth 9999))))
	(when template
	  (setf candidates
			(-filter (lambda (file) (pm-list-conforms-to template
														 (pm-locals-read-file file)))
					 candidates)))
	candidates))

(cl-defun pm-locals-find-caches-downwards (&optional (from-path default-directory) template)
  "Like pm-locals-find-cache, but searches downwards the
  directory-hierarchy instead of upwards. Returns the list sorted by depth"
  (let ((candidates
		 (->> dir-locals-directory-cache
			  (-map (-lambda (el) (car el)))
			  (-filter (-lambda (path)
						 (string-prefix-p (f-full from-path)
										  (f-full path))))
			  (-sort (-lambda (p1 p2) (< (s-count-matches "/" p1)
									(s-count-matches "/" p2))))
			  )))

	(dolist (cand candidates)
	  (when (not (pm-list-conforms-to template (pm-locals-read-cache cand)))
		(setf candidates (remove cand candidates))))
	candidates))

(cl-defun pm-locals-find-cache (&optional (from-path default-directory) template)
  "Find FROM-PATH, including itself, the first dir-locals cache
that satisfies TEMPLATE, if given, and return its directory."
  (cl-block pm-locals-find-cache
	(let ((candidates
		   (->> dir-locals-directory-cache
				(-map (-lambda (el) (car el)))
				(-filter (-lambda (path)
						   (string-prefix-p (f-full path)
											(f-full from-path))))
				(-sort (-lambda (p1 p2) (> (length p1)
									  (length p2)))))))
	  (dolist (cand candidates)
		(when (pm-list-conforms-to template (pm-locals-read-cache cand))
		  (cl-return-from pm-locals-find-cache
			cand))))
	(signal 'cache-missing (list "No dir-locals stash conforming to template found."
								 template
								 from-path))))

;; ---------------------------------------------------------

(cl-defun pm-locals-read-file (path)
  "Read .dir-locals.el from path and return list."

  (when (not (string= (f-filename path) ".dir-locals.el"))
	(setf path (concat (f-slash path) ".dir-locals.el")))
  (when (not (f-exists? path))
	(signal 'file-missing '(path)))

  (with-temp-buffer
	(insert-file-contents-literally path)
	(condition-case-unless-debug nil
		(read (current-buffer))
	  (end-of-file nil))))

(cl-defun pm-locals-read-cache (dirpath)
  (dir-locals-get-class-variables (intern
								   (s-collapse-whitespace
									(f-slash dirpath)))))

(cl-defun pm-locals-write-file (path stash &optional merge)
  "Write STASH to .dir-locals.el at PATH, creating it if
  necessary. If the file exists, and MERGE is nil, it is
  overwritten. If MERGE is t, STASH is merged into it."
  (when (not (f-exists? path))
	(signal 'file-missing '(path)))

  (let* ((path (f-slash path))
		 (ex-contents (ignore-errors (pm-locals-read-file (concat path
																  ".dir-locals.el")))))
	(when (and ex-contents merge)
	  (setf stash (pm-list-merge :from ex-contents :to stash)))

	(pm-locals--with-formatted-buffer stash
	  (write-file (concat path ".dir-locals.el")))))

(cl-defun pm-locals-write-cache (path new-stash &optional merge)
  "Writes STASH to the cache at PATH, creating it if necessary.
  If it exists, and MERGE is nil, it is overwritten. If MERGE is
  t, STASH is merged into it."
  (when (not (f-exists? path))
	(signal 'file-error '(path)))

  (let* ((path (f-slash path))
		 (path-sym (intern (s-collapse-whitespace path)))
		 (ex-stash (alist-get path-sym dir-locals-class-alist)))
	(when (and ex-stash merge)
	  (setf new-stash (pm-list-merge :from ex-stash :to new-stash)))
	(dir-locals-set-class-variables path-sym new-stash)
	(when (null ex-stash)
	  (dir-locals-set-directory-class path path-sym))))

(cl-defun pm-locals-delete-file (path)
  (let ((fpath (if (string= (f-filename path) ".dir-locals.el")
				   path (concat (f-slash path) ".dir-locals.el"))))
	(when (not (f-exists? fpath))
	  (signal 'file-error (list fpath)))
	(f-delete fpath)))

(cl-defun pm-locals-delete-cache (path)
  (let* ((path (f-slash path))
		 (path-sym (intern (s-collapse-whitespace path)))
		 (ex-stash (alist-get path-sym dir-locals-class-alist)))
	(when (not ex-stash)
	  (signal 'cache-missing (list path)))
	(setf dir-locals-class-alist
		  (map-delete dir-locals-class-alist path-sym))
	(setf dir-locals-directory-cache
		  (map-remove (lambda (key val) (string= key path)) dir-locals-directory-cache))))

;; ---------------------------------------------------------

(cl-defun pm-locals-update-buffers (&optional (root-path default-directory))
  "Update all live buffers below ROOT with new cache content.

Local variable filtering via `ignored-local-variables' etc. is
currently not used."

  ;; (message "updating buffers below %s" root)
  (when (not (f-exists? root-path))
  	(signal 'file-error (list "Cannot update buffers below, not existant.")))

  (let* ((root (f-slash root-path))
		 (root-cache-sym (intern (s-collapse-whitespace root)))
		 (root-cache-vars (cdr (assq root-cache-sym dir-locals-class-alist)))
		 root-cache-vars-cache
		 (mode-vars-cache (list))		; cached variables for a) cache b) mode
		 sub-caches)

	;; root-cache-vars-cache:  ((mmode1 . (..vars-alist..)) (mmode2 . (...vars-alist...)))
	;; mode-vars-cache: ((cache-path-sym1 (mmode1 . ((var1 . val)(var2 .val))) (mmode2 ...)))
	;; sub-caches:   (path1 path2 path3 ...)  (strings)

	;; sub-caches
	(dolist (cache-dir-el dir-locals-directory-cache)
	  (when (and (string-prefix-p root (car cache-dir-el))
				 (not (string= root (car cache-dir-el))))
		(push (car cache-dir-el) sub-caches)
		(push (list (cadr cache-dir-el)) mode-vars-cache)))

	;; sort in order they should be applied, by depth, top-levels first
	(setf sub-caches (sort sub-caches (lambda (c1 c2) (< (length (f-full c1))
													(length (f-full c2))))))

	;; ---- loop -----
	(dolist (buf (buffer-list))
	  (let ((buf-dir (f-slash (f-dirname (or (buffer-file-name buf)
											 default-directory)))))
		;; for every buffer below root:
		(when (string-prefix-p root buf-dir)
		  (let* ((mode (buffer-local-value 'major-mode buf)))

			;; apply root-cache, if existant:
			(when root-cache-vars
			  (let ((vars (cdr (assq mode root-cache-vars-cache))))
				(when (not vars)
				  (setf vars (pm--collect-dir-locals-for-mode root-cache-vars
															  mode))
				  (setf root-cache-vars-cache (cons (cons mode vars) root-cache-vars-cache)))
				(pm--apply-local-variables vars buf)))

			;; apply sub-caches, if necessary:
			(dolist (cache sub-caches)
			  (when (string-prefix-p cache buf-dir)
				(let* ((cache-sym (intern cache))
					   (cache-cache (cdr (assq cache-sym mode-vars-cache)))
					   (mode-vars (cdr (assq mode cache-cache))))
				  (when (not mode-vars)
					(setf mode-vars
						  (pm--collect-dir-locals-for-mode (cdr (assq cache-sym dir-locals-class-alist))
														   mode))
					(setf (cdr (assq cache-sym mode-vars-cache))
						  (cons (cons mode mode-vars) cache-cache)))
				  (pm--apply-local-variables mode-vars buf))))))))))

(cl-defun pm-locals-update-here ()
  "Update pm-locals in the current buffer, if possible."

  ;; find every stash (cache) from here until hitting main pm stash
  ;; then, stop

  (let ((stashes (list (ignore-errors
					   (pm-locals-find-cache default-directory))))
		(do-search t)
		(main-found nil))
	(when (and (not (null (car stashes)))
			   (pm-locals--is-main-stash (pm-locals-read-cache (car stashes))))
	  (setf do-search nil)
	  (setf main-found t))
	(while do-search
	  (let ((stash (ignore-errors
					 (pm-locals-find-cache (f-parent (car stashes))))))
		(if stash
			(progn (push stash stashes)
				   (when (pm-locals--is-main-stash
						  (pm-locals-read-cache (car stashes)))
					 (setf do-search nil)
					 (setf main-found t)))
		  (setf do-search nil))))

	(when main-found
	  (while stashes
	  	(pm--apply-local-variables
	  	 (pm--collect-dir-locals-for-mode
	  	  (cdr (assq (intern (s-collapse-whitespace (f-slash (pop stashes))))
					 dir-locals-class-alist))
	  	  major-mode))))))

(cl-defun pm-locals-set (vars &key
							  (category major-mode)
							  persist
							  sync-buffers)
  "Convenience function to set variables VARS in the
first cache or file up from `default-directory' that contains
CATEGORY, or the top-level pm stash, if it exists. If neither
does, an error is raised.

If PERSIST is T, the change is written to the file .dir-locals.el
at that position, creating it if necessary. Otherwise only the
cache is modified. If SYNC-BUFFERS is t, all affected buffers are
updated."

  (let* ((stash-dir
		  (or (ignore-errors
				(pm-locals-find-cache default-directory
									  `((,category))))
			  (ignore-errors
				(pm-locals-find-cache default-directory
									  `((nil . (,@pm-locals--main-stash-vars)))))
			  (signal 'stash-missing (list default-directory)))))

	(->> (pm-locals-read-cache stash-dir)
		 (pm-list-set-or-replace vars category)
		 (pm-locals-write-cache stash-dir))
	(when persist
	  (->> (pm-locals-read-file stash-dir)
		   (pm-list-set-or-replace vars category)
		   (pm-locals-write-file stash-dir)
		   ))
	(when sync-buffers
	  (pm-locals-update-buffers stash-dir))))

;; ---------------------------------------------------------

(defmacro pm-locals--with-formatted-buffer (form &rest body)
  "Creates a temporary buffer in which form has been properly
  formatted and input."
  (declare (indent defun))
  (let ((temp-buffer (make-symbol "temp-buffer")))
	`(let ((,temp-buffer (generate-new-buffer " *pm-temp*"))
		   (inhibit-message t))
	   (with-current-buffer ,temp-buffer
		 (unwind-protect
			 (progn
			   (delay-mode-hooks (emacs-lisp-mode))
			   (insert (pm--dir-locals-to-string ,form))
			   (indent-region (point-min) (point-max) nil)
			   (message "inside pm-locals--with-formatted-buffer...")
			   ,@body)
		   (and (buffer-name ,temp-buffer)
				(kill-buffer ,temp-buffer)))))))

(defun pm--dir-locals-to-string (variables)
  "Output alists of VARIABLES to string in dotted pair notation
  syntax."
  (format "(%s)"
		  (mapconcat
		   (lambda (mode-variables)
			 (format "(%S . %s)"
					 (car mode-variables)
					 (format "(%s)" (mapconcat
									 (lambda (variable-value)
									   (format "(%s . %s)"
											   (car variable-value)
											   (pp (cdr variable-value))))
									 (cdr mode-variables) "\n"))))
		   variables "\n")))

(cl-defmacro pm--with-every-buffer-below (dir &rest body)
  (declare (indent defun))
  `(dolist (buffer (buffer-list))
	 (with-current-buffer buffer
	   (when (and (buffer-file-name)
				  (s-prefix? ,dir default-directory))
		 ,@body))))

(defun pm--collect-dir-locals-for-mode (class-vars mode)
  "Collect all variables from CLASS-VARS that apply for
  major-mode MODE."
  (let (variables)
	(dolist (entry class-vars variables)
	  (let ((key (car entry)))
		(cond
		 ((stringp key)
		  ;; Don't include this in the previous condition, because we
		  ;; want to filter all strings before the next condition.
		  (when (and sub-file-name
					 (>= (length sub-file-name) (length key))
					 (string-prefix-p key sub-file-name))
			(setq variables (dir-locals-collect-variables
							 (cdr entry) root variables))))
		 ((or (not key)
			  (provided-mode-derived-p key mode))
		  (let* ((alist (cdr entry))
				 (subdirs (assq 'subdirs alist)))
			(if (or (not subdirs)
					(progn
					  (setq alist (delq subdirs alist))
					  (cdr-safe subdirs))
					;; TODO someone might want to extend this to allow
					;; integer values for subdir, where N means
					;; variables apply to this directory and N levels
					;; below it (0 == nil).
					(equal root default-directory))
				(setq variables (dir-locals-collect-mode-variables
								 alist variables))))))))))

(cl-defsubst pm--apply-local-variables (vars &optional (buf (current-buffer)))
  (dolist (elt vars)
	;; (unless (memq (car elt) '(eval mode))
	;;   ;; TODO:  how to implement the functionality for eval and mode?
	;;   ;; (setq dir-local-variables-alist
	;;   ;; 	  (assq-delete-all (car elt) dir-local-variables-alist))
	;;   )
	;; TODO: efficiency? better to do with-current-buffer, or like this?
	(setf (buffer-local-value (car elt) buf) (cdr elt))))

(defun +locate-dominating-file-downwards (dir name depth)
  "Like locate-dominating-file, but searches down the directory
hierarchy, DEPTH levels deep starting at DIR. If DEPTH is nil,
recurse indefinitely."
  (when (> depth 0)
	(let ((result nil) (subdirs nil))
	  (dolist (file (file-name-all-completions "" dir))
		(cond
		 ;; directory
		 ((and (directory-name-p file) (not (member file '("./" "../"))))
		  (setq subdirs (nconc subdirs (list (expand-file-name file dir)))))
		 ;; file
		 ((string= file name)
		  (setq result (nconc result (list (expand-file-name file dir)))))))
	  (dolist (subdir subdirs)
		(setq result (nconc result (+locate-dominating-file-downwards subdir name (- depth 1)))))
	  result)))

(cl-defun list-dirs-recursively (dir depth &optional include-symlinks)
  "Return list of subdirectories recursively which are at max DEPTH
levels nested deep below DIR. Returns absolute paths. Optionally
call recursively on symlinks."
  (when (> depth 0)
	(let ((result nil)
          (tramp-mode (and tramp-mode (file-remote-p (expand-file-name dir)))))
      (dolist (file (file-name-all-completions "" dir))
		(when (and (directory-name-p file) (not (member file '("./" "../"))))
          (setq result (nconc result (list (expand-file-name file dir))))
          (let* ((leaf (substring file 0 (1- (length file))))
				 (full-file (expand-file-name leaf dir)))
			;; Don't follow symlinks to other directories.
			(unless (and (file-symlink-p full-file) (not include-symlinks))
              (setq result
					(nconc result (list-dirs-recursively full-file (- depth 1))))))
          ))
	  result)))

(provide 'pm-locals)

