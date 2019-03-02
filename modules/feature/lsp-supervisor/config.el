;; modules/feature/lsp-supervisor/config.el -*- lexical-binding: t; -*-

(message "access/config.el ...")


(use-package feature/lsp-supervisor/lsps
  :after lsp-mode feature/cmake/cmake pjson
  :config

  (setq lsps--tools nil)
  (lsps-register-tool
   (make-lsps--tool
	:name 'lsp
	:dir-local-enabled-name 'lsp-enabled
	:major-modes '(c-mode c++-mode)
	:enable-fn-local #'(lambda ()
						 (lsp-ui-mode 1)
						 (lsp-ui-sideline-mode -1)
						 (make-local-variable 'company-backends)
						 (add-to-list 'company-backends 'company-lsp)
						 (lsp-mode 1)

						 ;; necessary: (why?)
						 ;; TODO: may fail in case there being mulitple clients in the current workspace
						 (lsp--find-workspace (lsp-session) (lsp--workspace-client (car lsp--buffer-workspaces)) (projectile-project-root))
						 )

	:disable-fn-local #'(lambda ()
						  (when lsp--buffer-workspaces
							(lsp-ui-mode -1)
							(make-local-variable 'company-backends)
							(setq company-backends (remove 'company-lsp company-backends))
							(lsp-mode -1)))
	:enable-fn-project #'(lambda (&optional project-root)
						   (when-let ((workspace (or (lsp--try-open-in-library-workspace)
													 (lsp--try-project-root-workspaces nil))))
							 (+utils-set-file-local-variable 'lsp--buffer-workspaces
															 workspace
															 project-root)))
	:disable-fn-project #'(lambda (&optional project-root)
							(let ((workspace (ignore-errors (or (lsp--try-open-in-library-workspace)
																(car (lsp--try-project-root-workspaces nil))))))
							  ;; NOTE: lsp--try-project-root-workspaces returns list, ignore for now
							  (when workspace
								(setf (lsp--workspace-shutdown-action workspace) 'shutdown)
								(with-lsp-workspace workspace (lsp--shutdown-workspace)))))
	:pre-enable-pred-fn #'(lambda (&optional project-root)
							;; TODO: here be dragons
							(message "1")

							(let* ((build-dir (+cmake-cdb-find-build-dir))
								   (source-dir (f-slash (if project-root project-root (projectile-project-root))))
								   (source-dir-db (concat (f-slash source-dir) "compile_commands.json"))
								   (source-dir-has-correct-db (and (f-readable? source-dir-db)
																   (f-file? source-dir-db)
																   (> (f-size source-dir-db) 0)
																   (if (f-symlink? source-dir-db)
																	   (f-same? (f-dirname (file-chase-links source-dir))
																				build-dir)
																	 t))))
							  (message "2")
							  (if (not source-dir-has-correct-db)
								  (progn
									(message "3, trying to create compilation db for source-dir %s from %s" source-dir build-dir)
									(ignore-errors (f-delete source-dir-db))
									(+cmake-create-compilation-db
									 :build-dir build-dir
									 :target-dir source-dir
									 (funcall #'(lambda ()
												  (message "Tried creating compile_commands.json for %s from build-dir %s. Succeded: %s"
														   source-dir build-dir (f-readable?  source-dir-db)))))
									;; return nil, not valid yet, cmake is running
									nil)
								(message "Found valid compile_commands.json in %s" source-dir " .")
								;; found, return t, valid
								t))

							;; TODO: find out if it is necessary to do something when pjson-target changes
							;; (-> freshen index or something?)
							)
	)
   )

  ;; -> put some code into some ycmd module
  (lsps-register-tool
   (make-lsps--tool
	:name 'ycmd
	:dir-local-enabled-name 'ycmd-enabled
	:major-modes '(c-mode c++-mode)
	:enable-fn-local #'(lambda ()
						 (ycmd-mode 1)
						 (ycmd-eldoc-mode 1)
						 (make-local-variable 'company-backends)
						 (dolist (sym '(company-ycmd company-files))
						   (add-to-list 'company-backends sym))
						 )
	:disable-fn-local #'(lambda ()
						  (ycmd-mode -1)
						  (ycmd-eldoc-mode -1)
						  (make-local-variable 'company-backends)
						  (dolist (sym '(company-ycmd company-files))
							(setf company-backends (remove sym company-backends)))
						  )
	:all-projects-closed-fn #'(lambda () (ycmd-close))))


  )



