;;; modules/feature/pm/config.el                   -*- lexical-binding: t; -*-

(message "feature/pm/config.el ...")

(use-package pm
  ;; :load-path (concat (getenv "DIR_SYSTEM") "/emacs/modules/feature/pm/")
  :load-path (lambda () (concat dir_emacs "/modules/feature/pm/"))
  ;; :after (ivy ido counsel projectile counsel-projectile f magit)
  ;; :demand
  :init

  :config
  (+load "pm-locals.el")
  (+load "pm.el")

  (add-hook 'find-file-hook #'pm-locals-update-here nil)

  ;; TODO:  move to pm-locals, implement there, proper, for sub-caches, too
  (defvar pm-locals--on-changed-hook nil
	"Hook called when pm-locals have changed. Passes on the
	directory the changed stash belongs to, as well as old and
	new content.")

  ;; TODO:  implement into pm-locals proper. Want this implemented automatically
  (defun +pm-locals-update (&optional dir)
	"Updates pm-locals from main-stash as found looking in and
	upwards of DIR. Then, calls +pm-locals--on-changed-hook with
	old and new contents, allowing to specifically act on changes.
        TODO: here only for main files, implement also for caches and sub-stashes!"
	(interactive)
	;; update cache    TODO: currently only works for main-stash .dir-locals.el, do fix that
	(let* ((main (ignore-errors (pm-locals-find-main-file)))
		   (content-new (when main (pm-locals-read-file main)))
		   (content-old (pm-locals-read-cache (f-dirname main))))
	  (when main
		(message "+pm-locals-update -- running hooks...")
		(run-hook-with-args 'pm-locals--on-changed-hook main content-old content-new)
		(pm-locals-write-cache default-directory
							   (pm-locals-read-file main))
		(pm-locals-update-buffers))))

  ;; -- update cache and files on saving .dir-locals.el
  (defun +pm-update-on-save-dir-locals ()
	(when (string= (f-filename (buffer-file-name))
				   ".dir-locals.el")
	  (+pm-locals-update)))
  (add-hook #'after-save-hook #'+pm-update-on-save-dir-locals)

  ;; -- inital load of the top-level main stashes of all projectile projects
  (run-with-timer 2 nil
				  (lambda ()
					(mapcar #'(lambda (p)
								(when (f-exists? (concat (f-slash p) ".dir-locals.el"))
								  (+pm-locals-update (f-expand p))))
							projectile-known-projects)))

  ;; -- TODO:  hooks
  ;;  * pm-locals--on-changed-hook
  ;;    - called once when a .dir-locals file (or cache?) is used for an update
  ;;    - ... allows for a comparison between old and new?
  ;;  * pm-locals--on-changed-for-file-hook
  ;;    - called for every file affected by a .dir-locals update


  )


