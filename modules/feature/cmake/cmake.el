;; -*- lexical-binding: t -*-
;; modules/feature/compilation/cmake.el

(defvar +cmake-run-command "cmake" "The command to run cmake
with. Can contain commands to setup the environment before the
call to cmake.")

(defvar +cmake-pre-script-path nil "Path to a script that is
passed to cmake using -C prior to all calls.")

(cl-defun +cmake-gui (&optional directory)
  (let ((default-directory (if directory directory default-directory)))
	(start-process "cmake" nil "cmake-gui" ".")))

(cl-defun +cmake-read-cache-variable (path var)
  "Read cmake cache-variable named var from cmake-cache-file at PATH.
Returns (name . val)"
  (when (not (f-exists? path))
	(error (concat "Path " path " does not exist.") ))
  (let* ((cmake-cache-file (if (f-file? path) path
							 (concat (f-slash path) "CMakeCache.txt"))))
	(with-temp-buffer
	  (insert-file-contents-literally cmake-cache-file)
	  (ignore-errors (re-search-forward (concat "\\(" var ":.*\\)=\\(.*\\)"))) nil t
	  (if (not (match-string 1))
		  nil
		`(,(match-string 1) . ,(match-string 2))))))

;; TODO:  generalize, make generalized utility fun out of it
(cl-defun +cmake-read-makefile-variable (path var)
  (when (not (f-exists? path))
	(error (concat "Path " path " does not exist.") ))
  (let* ((cmake-cache-file (if (f-file? path) path
							 (concat (f-slash path) "Makefile"))))
	(with-temp-buffer
	  (insert-file-contents-literally cmake-cache-file)
	  (ignore-errors (re-search-forward (concat "^" var " = \\(.*\\)")))
	  (if (not (match-string 1))
		  nil
		`(,var . ,(match-string 1))))))

;; TODO:  all these functions:  do notify somewhere if something goes wrong
;; at the moment I'm just ignoring it ("if not match-string 1" ..)
(cl-defun +cmake-change-cache-variable (path var val)
  "Change cmake cache-variable named var in cmake-cache-file at PATH."
  (when (not (f-exists? path))
	(error (concat "Path " path " does not exist.") ))
  (let* ((cmake-cache-file (if (f-file? path) path
							 (concat (f-slash path) "CMakeCache.txt"))))
	(when (not (f-exists? cmake-cache-file))
	  (error (concat "Path " cmake-cache-file " does not exist.") ))
	(with-temp-buffer
	  (insert-file-contents-literally cmake-cache-file)
	  (ignore-errors (re-search-forward (concat var ":.*=\\(.*\\)")))
	  (if (not (match-string 1))
		  nil
		(progn
		  (delete-region (match-beginning 1) (match-end 1))
		  (goto-char (match-beginning 1))
		  (insert val)
		  (write-file cmake-cache-file nil))))))

(defun +cmake--varlist-to-argstr (varlist)
  "Convert VARLIST consisting of (name . val) cells to string of
  the form -DNAME=val."
  (message (format "%s" varlist))
  (setf V varlist)
  (-reduce #'concat
		   (-map (-lambda ((name . val))
				   (concat "-D" name "=" (format "%s" val) " "))
				 (list varlist))))

(cl-defun +cmake-set-modify-cache-variables (build-dir varlist)
  "Set and/or modify cache variables of the cell-form (name .
  val) in list VARLIST in BUILD-DIR containing CMakeCache.txt.
  Calls cmake with -D using emacs compilation interface."
  (+cmake-run :build-dir build-dir
			  :argstr (+cmake--varlist-to-argstr varlist)))

(cl-defun +cmake-make-p (&optional (dir default-directory))
  "Returns t if project at dir is a make project."
  (-contains-p (directory-files dir) "Makefile"))

(cl-defun +cmake-ninja-p (&optional (dir default-directory))
  "Returns t if project at dir is a ninja project."
  (-contains-p (directory-files dir) "build.ninja"))

(cl-defun +cmake-make-to-ninja (build-dir)
  "Change generator from make to ninja."

  (let (compilation-finish-functions-cache
		(build-dir (if build-dir (f-slash build-dir) "")))
	(when (not  (+cmake-init-p build-dir))
	  (error "Cannot convert from make to ninja: CMake build folder not initialized."))

	(when (not (+cmake-make-p build-dir))
	  (error "Cannot convert from make to ninja: Make is not being used."))

	;; extract CMAKE-SOURCE_DIR from Makefile
	(let ((cmake-source-dir (cdr (+cmake-read-makefile-variable
								  build-dir
								  "CMAKE_SOURCE_DIR" ))))
	  ;; store old cmake cache
	  (ignore-errors (f-delete (concat build-dir ".TEMP_CMakeCache")))
	  (f-copy (concat build-dir "CMakeCache.txt") (concat build-dir ".TEMP_CMakeCache" ))

	  ;; clean up
	  (+cmake-clean build-dir)

	  ;; run cmake with ninja generator, then continue
	  (+cmake-init :source-dir cmake-source-dir :build-dir build-dir :generator 'ninja
				   ;; replace CMAKE_MAKE_PROGRAM in make cachefile with ninja cache file value
				   (let ((cmake-make-program-ninja (cdr (+cmake-read-cache-variable build-dir "CMAKE_MAKE_PROGRAM"))))

					 (when (not (f-exists? cmake-make-program-ninja))
					   (error (concat "Cannot convert from make to ninja:  ninja make program at "
									  cmake-make-program-ninja
									  "does not exist.")))
					 (when (f-exists? (concat build-dir ".TEMP_CMakeCache"))
					   (ignore-errors (f-delete (concat build-dir "CMakeCache.txt")))
					   (f-move (concat build-dir ".TEMP_CMakeCache") (concat build-dir "CMakeCache.txt"))
					   (+cmake-change-cache-variable build-dir "CMAKE_MAKE_PROGRAM" cmake-make-program-ninja)
					   (+cmake-run :build-dir build-dir)))))))

;; TODO: run these shell-commands in a compilation buffer, they do invoke cmake
;; sometimes, which may screw up code running afterwards that expects the
;; directories to have been cleaned up already.
(cl-defun +cmake-clean (dir)
  "Wipe directory DIR of its cmake configuration."
  (let* ((dir (f-slash dir))
		 (default-directory dir))
	;; clean up build
	(cond ((+cmake-make-p dir) (shell-command "make clean"))
		  ((+cmake-ninja-p dir) (shell-command "ninja clean")))
	;; delete generator files
	(->> (-intersection '("CMakeFiles" "Makefile" "build.ninja" "CMakeCache.txt")
						(directory-files dir))
		 (-map (-lambda (filename) (f-delete
									(concat dir filename)
									t)))))
  t)

(cl-defun +cmake-init-p (&optional (dir default-directory))
  "Returns t if directory contains a cmake configuration."
  (let ((cmake-files '("CMakeFiles" "CMakeCache.txt")))
	(eq (length cmake-files)
		(length (and (-intersection cmake-files
									(directory-files dir)))))))

(cl-defmacro +cmake-run (&rest body &key
							   (source-dir nil)
							   (build-dir nil)
							   (argstr "")
							   &allow-other-keys)
  "Evokes +cmake-run, then -if given- runs &body once after it has finished
successfully. Requires `compilation-finish-functions' to not be
changed during execution."
  ;; TODO:  fix this fuck, seems like arguments (directories) are baken into this on macro expansion.
  `(if (null ,body)
	   (+cmake--do-run :source-dir source-dir
					   :build-dir build-dir
					   :argstr ,argstr)
	 (let ((compilation-finish-functions-before compilation-finish-functions))
	   (cl-labels ((run-after-fun (BUF MESSAGE)
								  ,@body
								  (remove-hook #'compilation-finish-functions #'run-after-fun)))
		 (add-hook 'compilation-finish-functions #'run-after-fun)

		 (+cmake--do-run :source-dir source-dir
						 :build-dir build-dir
						 :argstr ,argstr)))))

(cl-defun +cmake--do-run (&key (source-dir default-directory)
							   (build-dir default-directory)
							   (argstr ""))
  ;; build-dir contains configuration?
  ;;  y -> run cmake with build-dir and argstr, disregard source-dir
  ;;  n -> run cmake in build-dir with source-dir and argstr
  (let ((default-directory build-dir))
	(if (+cmake-init-p)
		(compile (concat +cmake-run-command " . " argstr))
	  (compile (concat +cmake-run-command " " source-dir " " argstr)))))

(cl-defmacro +cmake-init (&rest body &key
								(source-dir default-directory)
								(build-dir default-directory)
								(args "")
								(generator 'ninja)
								(force nil)
								&allow-other-keys)
  "Like +cmake--do-init, but executes BODY once after cmake has
  finished after evaluation."
  `(let* ((default-directory ,build-dir)
		  (genstr ,(pcase generator
					 ('make "-G 'Unix Makefiles'")
					 ('ninja "-G 'Ninja'")))
		  (varstr (cond ((listp ,args) (+cmake--arglist-to-argstr ,args))
						((stringp ,args) ,args))))
	 ;; cmake configuration exists? clean if force, error otherwise
	 (when (+cmake-init-p)
	   (if ,force
		   (+cmake-clean ,build-dir)
		 (error (concat "Directory " ,build-dir " already contains a \
cmake configuration. Pass :force t to clean before (re-)initialization."))))
	 (+cmake-run
	  :build-dir ,build-dir
	  :source-dir ,source-dir
	  :argstr (concat genstr " " varstr)
	  ,@body)))

;; ----------
;; 1) checking if db existant:
;;    - get source-dir from pjson or elsewhere  (...need be smart)
;;    - get build-dir from pjson, if possible
;;    - guess at build-dirs for various build-dir/source-dir combinations
;;

(defun +cmake-cdb--build-dir? (dir &optional subdir)
  "Check if dir/subdir is a valid build dir (with trailing slash),
or contains a valid subdirectory `build'."
  ;; TODO: ninja?
  (let ((dir (concat (f-slash dir) subdir)))
	(cond ((f-exists? (concat dir "CMakeCache.txt"))
		   (message "found here %s" dir)
		   (f-slash dir))
		  (t nil))))

(defun +cmake-cdb-find-build-dir ()
  "Best-guess cmake build-directory upwards from
default-directory, or pjson build-directory, the latter of which
which takes precedence."
  (let* ((pjson-project (ignore-errors (pjson-current-project)))
		 (pjson-build-dir (ignore-errors (pjson-get
										  `(targets
											,(pjson-get-active-target)
											build_dir)
										  pjson-project))))
	(if (and pjson-project
			 (f-exists? (concat pjson-build-dir "CMakeCache.txt")))
		pjson-build-dir
	  (cond
	   ((if-let ((res (f-traverse-upwards (lambda (d)
											(+cmake-cdb--build-dir? (f-slash d) "build/"))
										  default-directory)))
			(concat (f-slash res) "build/")
		  nil))
	   ((f-traverse-upwards #'+cmake-cdb--build-dir?
							default-directory))))))

(cl-defun +cmake-dir-has-comp-db-config (&optional (build-dir default-directory))
  "Indicates whether BUILD-DIR cmake configuration has has
CMAKE_EXPORT_COMPILE_COMMANDS set to 1 or ON."
  (cl-flet ((conf-valid (name . val)
						(or (string= val "ON") (string= val "1"))))
	(-some->
	 (+cmake-read-cache-variable (concat (f-slash build-dir)
										 "CMakeCache.txt")
								 "CMAKE_EXPORT_COMPILE_COMMANDS")
	 conf-valid)))

(cl-defmacro +cmake-create-compilation-db (&rest body &key
												 (build-dir default-directory)
												 (target-dir nil)
												 &allow-other-keys)
  "Creates compilation-db for build-dir in BUILD-DIR and symlink
to TARGET-DIR, if given."
  `(let* ((default-directory ,build-dir)
		  (build-dir ,build-dir)
		  (db-path-source (concat (f-slash ,build-dir) "compile_commands.json"))
		  (comp-exit-fun compilation-exit-message-function)
		  (target-dir (if ,target-dir (f-slash ,target-dir) nil))
		  (db-path-target (concat target-dir "compile_commands.json")))

	 (message "Inside +cmake-create-compilation-db, build-dir: %s default-directory: %s target-dir: %s" build-dir default-directory target-dir)
	 ;; -- cmake dir already initialized?
	 (when (not (+cmake-init-p))
	   (error (concat "CMake dir '" build-dir "' is not initialized.")))

	 ;; -- install handler to install symlink after compilation compilation, and run ,@body
	 (cl-flet ((do-symlink (proc-status exit-status exit-message)

						   ;; reset handler
						   (setf compilation-exit-message-function comp-exit-fun)

						   (if (eq exit-status 0)
							   ;; cmake process ran successfully:
							   (progn
								 ;; remove file/symlink if existing (could point somewhere else)
								 (when (or (f-exists? db-path-target)
										   (f-symlink? db-path-target))
								   (f-delete db-path-target))
								 (f-symlink (f-long db-path-source) (f-long db-path-target))
								 ;; run body
								 (message "  Created and symlinked compile_commands.json from %s to %s" db-path-source db-path-target)
								 ,@body
								 '("" . ""))
							 ;; cmake process error:
							 (error (concat "Failed generating compile_commands.json in '"
											build-dir
											"'. exit-message: " exit-message)))
						   ))

	   ;; -- ensure CMAKE_EXPORT_COMPILE_COMMANDS is set
	   (when (not (+cmake-dir-has-comp-db-config))
		 ;; set variable, and re-run cmake
		 (+cmake-set-modify-cache-variables build-dir
											'("CMAKE_EXPORT_COMPILE_COMMANDS" . "ON")))
	   ;; run cmake
	   (+cmake-run)

	   ;; -- install handler to symlink after compilation process is done
	   (when (and target-dir)
	   	 (setf compilation-exit-message-function #'do-symlink))
	   nil
	   )

	 )
  )


(provide 'feature/cmake/cmake)



