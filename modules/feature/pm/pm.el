;;; modules/feature/pm/pm.el                   -*- lexical-binding: t; -*-

(+load "pm-errors.el")
(+load "pm-list.el")
(+load "pm-locals.el")

;;;; public interface

;; ---------------------------------------------------------
;;;;; creation, display
(cl-defun pm-create (&key name root meta-dir prompt-prefix)
  "Creates a pm-project from NAME ROOT META-DIR. Will query the
user for missing arguments. And sync all buffers upon creation."
  (let* ((name
		  (if name (cond ((symbolp name) name)
						 ((stringp name) (intern name))
						 (t (error "Invalid project name: %s" name)))
			(pm--query (concat prompt-prefix "project-name> ") "" 'symbol)))
		 (proot (projectile-project-root))
		 (root
		  (if root (if (f-exists? root) root
					 (when (y-or-n-p (format "Root directory '%s' does not exist. Create?" root))
					   (make-directory root t)
					   root))
			(pm--query (concat prompt-prefix "root> ") (if proot proot default-directory) 'directory)))
		 (meta-dir
		  (if meta-dir (if (f-exists? meta-dir) meta-dir
						 (when (y-or-n-p (format "Meta-dir directory '%s' does not exist. Create?" meta-dir))
						   (make-directory meta-dir t)
						   meta-dir))
			(pm--query (concat prompt-prefix "meta-dir> ") root 'directory)))))

  (pm-locals-create root (pm-locals--main-stash-create root name meta-dir))
  (pm-locals-update-buffers root))


;; ---------------------------------------------------------
;;;;; compilation, build-management
(defun pm-compile (&optional starget)
  "Compiles STARGET in current project-context - if provided. If
it is not, queries user with GUI for interactive selection."
  (interactive)

  (condition-case err
	  (let* ((project-name (if (ignore-errors project-name) project-name
							 (error "No pm context, project-name missing."))))

		(when (null starget)
		  (ivy-read (concat "[" project-name "] compile: ")
					(pm--targets :sorted t :alist t)
					:action #'(-lambda ((_ . selection))
								(setf starget selection))))

		;; (when (null (seq-contains targets starget))
		;;   (error "pm-compile:  Target %s does not exist in targets list. Cannot compile.") starget)

		;; start actual compilation
		starget

		)
	(error (format "pm-compile:  %s" (error-message-string err))))
  )

(defun pm-recompile ()
  "(Re-)compiles current target if it exists.")

(defun pm-open-build-configuration (&optional starget)
  "Opens build-configuration for current target, or the STARGET
  given. May be an external tool e.g. cmake for C++, or a
  configuration file.")

(defun pm-change-target (&optional starget)
  "Changes the current target if STARGET is given, prompts for
  the target otherwise."
  )

;; ---------------------------------------------------------
;;;;; code-completion

(defun pm-enable-completion ()
  "Enable code-completion in the current context. Performs the
  following steps:
    - determines target-type
    - collects information about current environment
    - provides user selection of tools to select from, if there is more than one
    - checks if the environment is suitable for the select tool,
      trying to correct it if it is not
    - enables the tool for all current and future relevant buffers
"
  )

;; ---------------------------------------------------------
;;;;; state management
(cl-defun pm-set-variables (&key vars	;
								 (category nil)
								 (path default-directory)
								 (local nil)
								 (persist nil)
								 (update-buffers t))
  "Set variables from dottest-list VARS in a stash, i.e. a cache
or file. If LOCAL is nil, look for the nearest pm dir-locals
stash in parent directories from path, otherwise write the
changes into the stash at PATH, creating it if necessary. If
PERSIST is t, persist the changes to the .dir-locals.el file. If
UPDATE-BUFFERS is t, update dir-locals in all buffers below."

  ;; NOTE: initially, assume proper initialization i.e. a cache exists at path.

  ;; find directory, if possible, if necessary    ->   macro pm--with-stash
  (pm-locals--with-stash
   path local persist
   (progn
	 ;; write cache

	 ;; if persist, write file
	 ;;   yes -> persist?
	 ;;          yes -> create .dir-locals.el at path, load
	 ;;          no  -> create cache at path, "load"

	 ;; if update-buffers, do that
	 )
   )

  )

(cl-defun pm-delete-variables (&key vars
									(category nil)
									(path default-directory)
									(local nil)
									(persist nil)
									(update-buffers t))
  "Like pm-set-variables, but deletes variables VARS given a list
  of symbols."
  ;; find directory, if possible, if necessary    ->   macro pm--with-stash
  (pm--with-stash
   path local persist
   (progn
	 ;; write cache

	 ;; if persist, write file

	 ;; if update-buffers, do that
	 )))

(cl-defun pm-synchronize-variables (&optional all-vars)
  "Synchronize in-memory and .dir-locals.el state. If ALL-VARS is
  t, variables that exist in-memory, but not in .dir-locals.el,
  are synchronized as well. Otherwise, only those already present
  in .dir-locals.el are written."

  )

;; ---------------------------------------------------------
;;;;; helpers for public interface

;;;; private
;;;;; ui

(defun pm--query (prompt initial type)
  "Query for TYPE with PROMPT, checks validity, and returns the
  string input if it is."
  (let ((valid nil) ret)
	(pcase type
	  ('directory (while (not valid)
					(let ((dpath (read-directory-name prompt initial)))
					  (if (f-exists? dpath)
						  (progn
							(setf valid t)
							(setf ret dpath))
						(when (y-or-n-p (format "Directory '%s' does not exist. Create?" dpath))
						  (make-directory dpath t)
						  (setf valid t)
						  (setf ret dpath))))))
	  ('file (while (not valid)
			   (let ((fpath (read-file-name prompt initial)))
				 (if (f-exists? fpath)
					 (progn
					   (setf valid t)
					   (setf ret fpath))
				   (when (y-or-n-p (format "File '%s' does not exist. Create?" fpath))
					 (find-file-noselect fpath t)
					 (setf valid -t)
					 (setf ret fpath))))))
	  ('symbol (setf ret (intern (read-string prompt initial))))
	  ('string (setf ret (read-string prompt initial))))
	ret))


;;;;; target management

(cl-defun pm--targets (&key sorted strings alist)
  "Returns list of targets. If SORTED is true, returns them
  sorted by recency (TODO), with the current-target on top. If
  STRINGS is t, returns targets as strings which may contain
  additional meta information and fontification. If ALIST is
  true, returns targets as an alist with elements (STARGET .
  STRING)."
  ;; map-keys
  ;; order
  ;; stringified

  ;; TODO: when targets not defined -> dispatch to some diagnosis defun
  ;; (what is it exactly, that is missing here?)

  (let* ((target-list (map-keys targets))
		 (ret target-list) )
	(when sorted
	  (setf target-list (pm--targets-sort target-list))
	  (setf ret target-list))
	(when (or strings alist)
	  (setf ret
			(mapcar (lambda (starget)
					  (->> starget
						   (pm-target-create)
						   (pm-target--as-string)))
					target-list)))
	(when alist
	  (setf ret (-zip ret target-list)))
	ret))

(cl-defun pm--targets-sort (targets)
  ;; NOTE: no recency sorting using pcache yet
  (if (pm--current-target)
	  (cons current-target (remove current-target targets))
	targets))

(defun pm--current-target ()
  "Returns current target as symbol, if it exists."
  (if current-target
	  (if (seq-contains targets current-target)
		  current-target
		(progn
		  (message "pm--current-target:  current-target %s does not exist,\
setting current-target to nil." current-target)
		  (setq current-target nil)
		  nil))
	nil))

(defun pm--targets-ordered ())




(provide 'pm)
