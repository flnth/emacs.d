;; modules/feature/compilation/pjson.el

;;; json

(require 'json)
(setq pjson-file-path (concat dir_emacs 
						   "share/projects."
						   (system-name)
						   ".json"))

;; TODO:  integrate into my json-library proper:  file does not exist
(when (not (f-exists? pjson-file-path))
	(with-temp-buffer
	 (insert "{}") 
	  (write-file pjson-file-path)))

(setq pjson-file-contents nil)
(setq pjson-last-access 0)

(defun pjson-last-mod ()
  (cadr (nth 5 (file-attributes pjson-file-path)))
  )

(defun pjson-load ()
  "Loads projects from json file if a) not yet happened, b) json
file changed since last reload. Also tries to load cmake variables."
  (when (or (null pjson-file-contents)
		  (> (pjson-last-mod) pjson-last-access))
	(setq pjson-file-contents (json-read-file pjson-file-path))
	(setq pjson-last-access (nth 1 (current-time)))
	)
  pjson-file-contents)

;; some utility function
(defun pjson-known-projects ()
  (cl-loop for p in (pjson-load) collect (car p)))

(defun fn-alist-get (key-lst alist)
  (cl-labels ((lin-search (keys content)
					  (if (null content)
						  nil             ;; not found, return nil
						(if (null keys)
							(cdr content) ;; found, return value
						  (lin-search
						   (cdr keys)
						   (assoc (car keys) content))))))
	(lin-search key-lst alist)))

(defun pjson-get (search-keys project)
  "Navigate pjson file. E.g. to get cmakelists path for target
ires-iloadout for project 00810:
(fn-get '(targets ires-iloadout cmakelists) '00810')"
  (let* ((project (intern project))
		 (content (assoc project (pjson-load))))
	(fn-alist-get search-keys content)))

(defun pjson-get-targets-for (project)
  "Collect target strings for project"
  (cl-loop for target in (pjson-get '(targets) project) collect (car target)))

(defun pjson-get-targets ()
  (pjson-get-targets-for (pjson-current-project)))

(defun pjson-project-from-source-dir (source-dir)
  (cl-loop for p in (pjson-load) do
		   (let ((project-source-dir (cdr (assoc 'source_dir (cdr p)))))
			 (when (and (not (null project-source-dir))
						(string= (directory-file-name project-source-dir ) source-dir))
			   (cl-return (symbol-name (car p)))))))

(defun pjson-current-project ()
  ;; TODO:  error handling in pjson in general... wazup? Added ignore-errors here.
  (ignore-errors
	(pjson-project-from-source-dir (directory-file-name (projectile-project-root)))))

(defun pjson-get-for-current (search-elements)
  "Get value for the project that has same source directory as
projectile source."
  (let ((project (pjson-current-project)))
	(when (not (null project))
	  (pjson-get search-elements project))))

;; An association list that returns the target that was last active for that
;; project. If project is new, add target nil to the list.

;;;;; projectile interop

;; ensure that all porg projects are known to PROJECTILE and FASD
(let ((pjson-content (pjson-load))
	  (source-dirs (remove nil (loop for pname in (mapcar 'symbol-name (pjson-known-projects))
									 collect (ignore-errors (string-trim (let ((default-directory
																				 (pjson-get '(source_dir) pname)))
																		   (shell-command-to-string "git rev-parse --show-toplevel")))))))
	  (meta-dirs (remove nil (loop for pname in (mapcar 'symbol-name (pjson-known-projects))
								   collect (pjson-get '(meta_dir) pname)))))
  ;; -- source directories:  (projectile,fasd)
  (cl-loop for dir in source-dirs do
		   (let ((dir-home-subst (s-replace (getenv "HOME") "~" dir)))

			 ;; projectile
			 (when (not (seq-find
						 #'(lambda (s) (or (string= s dir)
									  (string= s dir-home-subst )))
						 projectile-known-projects))

			   ;; add if neither dir, nor dir with $HOME substituted to ~, is known to projectile
			   (add-to-list 'projectile-known-projects dir-home-subst))

			 ;; fasd
			 (shell-command (concat  "fasd -A " dir))))
  ;; meta-directories:  (fasd)
  (cl-loop for dir in meta-dirs do
		   (when (f-directory? dir)
			 (shell-command (concat "fasd -A " dir ))))
  )

;; TODO: ensure that all subdirectories in projects are known to fasd  (if necessary...)


;;;; active targets
;;;;; serialization
(setq pjson-active-targets-path
	  (concat dir_emacs "share/projects.active.targets." system-name))

(defun pjson-write-active-targets ()
  (f-write-text (format "%S" pjson-active-targets) 'raw-text pjson-active-targets-path))

(defun pjson-read-active-targets ()
  (when (f-exists? pjson-active-targets-path)
	(read (f-read-text pjson-active-targets-path 'raw-text))))

(setq pjson-active-targets (pjson-read-active-targets))


;;;;; access
(defun pjson-get-active-target (&optional project)
  (let ((project (if (null project)
					 (pjson-current-project)
				   project)))
	(if (not (null project))
		(alist-get (intern (pjson-current-project)) pjson-active-targets)
	  nil)))

(defun pjson-set-active-target (target)
  "Sets passed symbol/string as active target for current project to our
assoc list. Adds project if it does not yet exist. Schedules a
disk-write on first idle. Changes environment for new target."
  ;; pre-condition:  target exists
  (let ((target (if (stringp target) (intern target) target)))
	(setf (alist-get (intern (pjson-current-project)) pjson-active-targets) target)
	(let ((source-json (concat (pjson-get-for-current (list 'targets target 'build_dir)) "/compile_commands.json"))
		  (target-json (concat (projectile-project-root) "compile_commands.json")))
	  (when (f-exists? target-json)
		(f-delete target-json))
	  (if (f-exists? source-json)
		  (f-symlink source-json target-json)
		(message "%s %s %s %s" "-- pjson: no compile_commands.json found at\n   " source-json "\n  while changing active target to " target))
	  )
	(run-with-idle-timer 2 nil 'pjson-write-active-targets)
	t)
  )

(defun pjson-get-targets-for-project-sorted (project)
  "Gets targets for project, with the active target in front."
  (let ((project (pjson-current-project))
		(active-target (pjson-get-active-target))
		(targets (pjson-get-targets-for project)))
	(if (null active-target)
		targets
	  (progn
		(setq targets (remove active-target targets))
		(push active-target targets)))
	))

;;;; compilation commands

(defun pjson-compile (project target)
  "Start compilation on target."
  (interactive)
  (let* ((cur-buf (current-buffer)) 	; TODO: maybe not smart, need in top-level fun
	 (build-cmd (pjson-get (list 'targets target 'build_cmd) project))
	 (proot (pjson-get '(source_dir) project))
	 (default-directory (pjson-get (list 'targets target 'build_dir) project))
	 (shell-command-switch "-ic"))
	(if (null build-cmd)
		(message "%s" "No build_cmd found.")
	  (progn
		;; save projectile buffers and current buffer (ignored by projectile)
		(let* ((default-directory proot)
			   (projectile-root (projectile-project-root)))
		  (when projectile-root
			(projectile-save-project-buffers)
			(when (string-prefix-p projectile-root (buffer-file-name cur-buf))
			  (save-buffer cur-buf))))
		(compilation-start build-cmd)
		(setq pjson-last-compile-target (cons project target))))))

(defun pjson-compile-current-target ()
  "Compile current target of current project context."
  (interactive)
  (let ((project (pjson-current-project))
		(target (pjson-get-active-target)))
	(pjson-compile project target)))

(setq pjson-last-compile-target nil)

(defun pjson-compile-last-target ()
  "Compile last target independent of project context."
  (interactive)
  (if (not(null pjson-last-compile-target ))
	  (pjson-compile (car pjson-last-compile-target )
						(cdr pjson-last-compile-target))
	(message "%s" "No project target in history.")
	)
  )

(defun pjson-ivy-select-target ()
  "Choose active target for current project.
Shows current target first."
  (interactive)
  (let ((project (pjson-current-project)))
	(ivy-read
	 (concat project " make active: ")
	 (pjson-get-targets-for-project-sorted project)
	 :action '(lambda (target)
				(pjson-set-active-target target)
				))))

(defun pjson-ivy-compile-target ()
  "Choose target for current project and compile.
Shows current target first."
  (interactive)
  (let ((project (pjson-current-project)))
	(if (null project)
		(message "Not in a known pjson project.")
	  (ivy-read
		 (concat project " compile: ")
		 (pjson-get-targets-for-project-sorted project)
		 :action '(lambda (target)
					(pjson-compile project (intern target))
					(pjson-set-active-target (intern target))
					)))))

(defun pjson-open-cmake-gui (project target)
  (let* ((default-directory (pjson-get (list 'targets target 'build_dir) project)))
	(+cmake-gui)))

(defun pjson-ivy-cmake-gui ()
  "Open cmake-gui for a target of the current project."
  (interactive)
  (let ((project (pjson-current-project)))
	(if (null project)
		(message "Not in a known pjson project.")
	  (ivy-read
	   (concat project " cmake-gui: ")
	   (pjson-get-targets-for-project-sorted project)
	   :action '(lambda (target)
				  (pjson-open-cmake-gui project (intern target))
				  )))))


;;;; handle cmake variables for directories

(defvar pjson-cmake-cache-var-names-indurad nil
  "List of cmake cache variables to extract for every indurad target."
  )

(setq pjson-cmake-cache-var-names-indurad
	  '("monolith_dir"))

(defvar pjson-cmake-cache-variables nil
  "Cache variables for every project and target as read in from all cmake-caches.
Has alist structure of '(project1 (target1 (var . val) (var . val) ... ))")

(defun pjson-read-cmake-cache-variable (project target var)
  "Read cmake cache-variable var for project and target."
  (let ((cmake-cache-path (concat (pjson-get `(targets ,target build_dir) project)
  								  "/CMakeCache.txt"))) ;
	(ignore-errors
  	  (+cmake-read-cache-variable cmake-cache-path var))))

(defun pjson-cmake-caches-read ()
  "Read in cmake-cache-variables for all projects and targets.
Invalidates previous values."
  (setq pjson-cmake-cache-variables nil)
  (cl-loop  ;; for every project
   for project in (mapcar 'symbol-name (pjson-known-projects)) do
   (when (string= (pjson-get '(type) project ) "Indurad")
	 (let (( project-list '() ))
	   (cl-loop ;; for every target
		for target in (pjson-get-targets-for project) do
		(let (( targets-list '() ))
		  (cl-loop ;; for every cmake variable
		   for var in pjson-cmake-cache-var-names-indurad do
		   ;; extract variable value from CMakeCache.txt,
		   (let ((val (cdr (pjson-read-cmake-cache-variable project target var))))
			 (when (not (null val))
			   ;; and store when applicable
			   (asoc-put! targets-list var val t))))
		  (asoc-put! project-list target targets-list t)))
	   (asoc-put! pjson-cmake-cache-variables project project-list))))
  pjson-cmake-cache-variables)

(defun pjson-get-search-paths ()
  "Determine and return search paths for current context.
NOTE:  currently, add monolith_dir."
  (let* ((project (pjson-current-project))
  		 (target (pjson-get-active-target project)))
  	(when (not (and (null project) (null target)))
	  (ignore-errors
		(list
  		 (f-slash (fn-alist-get `(,project ,target "monolith_dir") pjson-cmake-cache-variables)))))))

(defun pjson-init ()
  "Initializing run for pjson. Necessary for pjson-cmake-caches-read."
  (pjson-load)
  (pjson-cmake-caches-read) 			; TODO: find way to intelligently DWIM load this (or not?)
  nil
  )

(pjson-init)


;;; custom interactive compile (-> pjson)
;;;; compile-command-per-target
;;;;; serialization
(setq pjson-compile-commands-path
	  (concat dir_emacs "/share/projects.compile.commands." system-name))

(defun pjson-write-compile-commands ()
  (f-write-text (format "%S" pjson-compile-commands) 'raw-text pjson-compile-commands-path))

(defun pjson-read-compile-commands ()
  (when (f-exists? pjson-compile-commands-path)
	(read (f-read-text pjson-compile-commands-path 'raw-text))))

;;;;; access, model
(setq pjson-compile-commands (pjson-read-compile-commands))

;; TODO:  if file does not exist yet, this does not actually work
;; (setq pjson-compile-commands '((project1 (target1 ("command1")))))

;; TODO: extend to serialize more than one compile command, that is possible,
;; though I wouldn't know how to make use of that atm (-> use e.g. in C-n/C-p on SPC-c C)
(defun pjson-add-compile-command (command &optional project target)
  "Adds command to the association list of the current project
and target. Adds entries for current project and/or target if they
do not yet exist."
  (let* ((cur-proj (if project project (intern (pjson-current-project))))
		 (cur-target (if target target (pjson-get-active-target cur-proj))))
	(cond
	 ;; add project (and target and command) if project non-existant
	 ((null (alist-get cur-proj pjson-compile-commands))
	  (nconc pjson-compile-commands `((,cur-proj (,cur-target ,command)))))

	 ;; add target (and command) if target non-existant
	 ((null (fn-alist-get `(,cur-proj ,cur-target) pjson-compile-commands))
	  (nconc (alist-get cur-proj pjson-compile-commands) `((,cur-target ,command))))

	 ;; add command if both exist
	 (t
	  (setf (car (fn-alist-get `(,cur-proj ,cur-target) pjson-compile-commands)) command ))))

  (run-with-idle-timer 2 nil 'pjson-write-compile-commands))

(defun pjson-get-last-compile-command (&optional project target)
  (let* ((cur-proj (if project project (intern (pjson-current-project))))
		 (cur-target (if target target (pjson-get-active-target cur-proj))))
	(car (fn-alist-get `(,cur-proj ,cur-target) pjson-compile-commands))))


;;;; compile-command string modification, hook

;; TODO: move to general utils or something
(defun pjson--join-words-within-parens (words)
  (let ((inside-parens nil)
		(out '()))
	(loop for w in words
		  if inside-parens
		  do (progn
			   (setf (car out) (s-append (concat " " w) (car out)))
			   (when (s-ends-with? ")" w)
				 (setf inside-parens nil))
			   )
		  else
		  do (progn
			   (when (s-starts-with? "$(" w)
				 (setf inside-parens t))
			   (setf out (cons w out))))
	(nreverse out)))

(defun pjson--extract-token-from-string (string)
  "Removes and extracts token starting with $ from the string.
  Leaves over $ symbols. Returns list containing words and token."
  (let* ((words (s-split " " (s-collapse-whitespace string)))
		 (token '())
		 (tmp nil))
	;; join $(word1 word2 word3) back together
	(setf words (pjson--join-words-within-parens words))

	;; extract, replace
	(loop for w in-ref words
		  when (s-starts-with? "$" w) do
		  (progn
			(setf token (append token `(,(substring w 1))))
			(setf w "$")))
	`(,words ,token)))

(defun pjson--compile-eval (token)
  (let ((token-out "")
		(evaluate (lambda (x)
					(let ((res (eval (car (read-from-string x)))))
					  (cond
					   ((stringp res) res)
					   ((numberp res) (prin1-to-string res)))))))
	(cond
	 ;; variables -------------------------------------------
	 ((not (s-starts-with? "(" token))
	  (setq tt token)
	  (cond
	   ;; -- $proot-dir                (projectile)
	   ((s-starts-with? "proot-dir" token)
		(setq token-out (s-replace "proot-dir"
								   (s-chop-suffix "/"
												  (projectile-project-root))
								   token
								   )))
	   ;; -- $pbuild-dir     (active target, pjson)
	   ((s-starts-with? "pbuild-dir" token)
		(setq token-out (s-replace "pbuild-dir"
								   (s-chop-suffix "/"
												  (pjson-get-for-current `(targets ,(pjson-get-active-target) build_dir)))
								   token)))
	   ;; --pmeta-dir       (active project, pjson)
	   ((s-starts-with? "pmeta-dir" token)
		(setq token-out (s-replace "pmeta-dir"
								   (s-chop-suffix "/"
												  (pjson-get-for-current '(meta_dir)))
								   token)))
	   (t (setf token-out (funcall evaluate token)))))
	 ;; elisp function -------------------------------------
	 ((and (s-starts-with? "(" token)
		   (s-ends-with? ")" token))
	  (setq token-out (funcall evaluate token))
	  )
	 (t (display-warning 'pjson--compile-eval (format "Token not recognized: %s. Ignoring token..." token))))
	token-out))

(defun pjson--process-compile-command (command)
  "Substitutes the following statements in COMMAND:
- elisp variables $var
- elisp function statements $(fun)
- context-specific pjson variables:
    $proot-dir $pbuild-dir $source-dir"
  (destructuring-bind (parsed-words token) (pjson--extract-token-from-string command )
	(loop for tok in-ref token do
		  ;; eval token separately
		  (setf tok (pjson--compile-eval tok))
		  ;; replace
		  (loop for w in-ref parsed-words
				when (string= w "$") do
				(setf w tok)
				(return)))
	(s-join " " parsed-words)))

;;;;; hook:  compilation command read-in
(defun pjson--compilation-read-command (f command &rest args)
  "Present last project/active-target compile comand instad of
the default one, if possible."
  (let ((ret ""))
	(unless
  		(ignore-errors
  		  (let* ((last-command (fn-pjson-get-last-compile-command))
  				 (cmd (if last-command last-command command)))
  			(setq cc cmd)
  			(setf ret (apply f `(,cmd) args))))
  	  (message "No active pjson target in context.")
  	  (setf ret (apply f command args)))
	ret))


;;;;; hook  recompile

(defvar pjson--recompile nil)
(defvar pjson--last-evaluated-cmd nil)

(defun pjson-recompile-around (f &rest args)
  (setq pjson--recompile t)
  (apply f args))

;;;;; hook:  compilation start
(defun pjson--compilation-start-around (f command &rest args)
  "Evaluates the passed command before execution. Executes the
last-evaluated command on recompile."
  (if (and pjson--recompile
		   (not (null pjson--last-evaluated-cmd)))
	  (progn
		(apply f pjson--last-evaluated-cmd args)
		(setq pjson--recompile nil))
	(progn
	  (let ((cmd (pjson--process-compile-command command)))
		(apply f cmd args)
		(setf pjson--last-evaluated-cmd cmd)
		(pjson-add-compile-command command)))))



(provide 'feature/compilation/pjson)
