;;; modules/feature/pm/tests/test-pm-locals.el     -*- lexical-binding: t; -*-

(eval-and-compile
  (require 'ert nil t)
  (require 'ert "lib/ert"))

(defmacro sheq (expr val)
  `(should (equal ,expr ,val)))

(defmacro ierrs (&rest body)
  (let (ret)
	(dotimes (i (length body))
	  (push `(ignore-errors ,(pop body)) ret))
	`(progn
	   ,@(nreverse ret))))

;; ---------------------------------------------------------
(ert-deftest pm-locals-create-read ()
  (let ((default-directory "/tmp"))
	(ignore-errors
	  (pm-locals-delete-cache "/tmp/")
	  (pm-locals-delete-file "/tmp/"))
	(pm-locals-create "/tmp/" (pm-locals--main-stash-create "/tmp/"))

	(let-alist (pm-locals-read-file default-directory)
	  (sheq .nil.projectile-project-root "/tmp/")
	  (sheq .nil.current-target nil)
	  (sheq .nil.targets nil))
	(let-alist (pm-locals-read-cache default-directory)
	  (sheq .nil.projectile-project-root "/tmp/")
	  (sheq .nil.current-target nil)
	  (sheq .nil.targets nil))

	(->> default-directory
		 (pm-locals-read-cache)
		 (pm-list-set-or-replace (list (cons 'current-target 10)) 'nil)
		 (pm-locals-write-cache default-directory))

	(let-alist (pm-locals-read-cache default-directory)
	  (sheq .nil.projectile-project-root "/tmp/")
	  (sheq .nil.current-target 10))
	(let-alist (pm-locals-read-file default-directory)
	  (sheq .nil.current-target nil))

	(->> default-directory
		 (pm-locals-read-file)
		 (pm-list-set-or-replace (list (cons 'current-target 15)) 'nil)
		 (pm-locals-write-file default-directory))

	(let-alist (pm-locals-read-cache default-directory)
	  (sheq .nil.current-target 10))
	(let-alist (pm-locals-read-file default-directory)
	  (sheq .nil.current-target 15))
	))

;; ---------------------------------------------------------
(ert-deftest pm-locals-find-x ()
  (ierrs (pm-locals-delete-file "/tmp/")
		 (pm-locals-delete-file "/tmp")
		 (pm-locals-delete-cache "/tmp/")
		 (pm-locals-delete-cache "/tmp")
		 (f-delete "/tmp/subdir" t))
  (mkdir "/tmp/subdir/" t)

  ;; complete file and cache
  (pm-locals-create "/tmp/" (pm-locals--main-stash-create "/tmp/"))
  ;; incomplete file
  (let ((content '((nil . ((projectile-project-root . "la")
  						   (var . 5))))))
  	(pm-locals-write-file "/tmp/subdir/" content)
  	(pm-locals-write-cache "/tmp/subdir/" content))

  ;; search upwards in dir-hierarchy
  (let ((default-directory (concat "/tmp/subdir/")))
  	(sheq (pm-locals-find-file)
  		  "/tmp/subdir/.dir-locals.el")
  	(sheq (pm-locals-find-file default-directory)
  		  "/tmp/subdir/.dir-locals.el")
  	(sheq (pm-locals-find-file default-directory '((nil . (var))) )
  		  "/tmp/subdir/.dir-locals.el")
  	(should-error (pm-locals-find-file default-directory '((nil . (asdf)))))
  	(sheq (pm-locals-find-file default-directory '((nil . (current-target))))
  		  "/tmp/.dir-locals.el")
  	(sheq (pm-locals-find-file default-directory '((nil . (current-target . nil))))
  		  "/tmp/.dir-locals.el")

  	(sheq (pm-locals-find-cache)
  		  (concat "/tmp/subdir/"))
  	(sheq (pm-locals-find-cache default-directory)
  		  "/tmp/subdir/")
  	(sheq (pm-locals-find-cache default-directory '((nil . (var))) )
  		  "/tmp/subdir/")
  	(should-error (pm-locals-find-cache default-directory '((nil . (asdf)))))
  	(sheq (pm-locals-find-cache default-directory '((nil . (current-target))))
  		  "/tmp/")
  	(sheq (pm-locals-find-cache default-directory '((nil . (current-target . nil))))
  		  "/tmp/")
	)

  ;; find main
  (let ((default-directory (concat "/tmp/subdir")))
	(sheq (pm-locals-find-main)
		  "/tmp/"))

  ;; -- search downwards in dir-hierarchy
  ;; files
  (let ((default-directory (concat "/tmp/subdir")))
	(sheq (pm-locals-find-file-downwards default-directory nil 0)
		  nil)
	(sheq (pm-locals-find-files-downwards default-directory nil 1)
		  (list "/tmp/subdir/.dir-locals.el"))
	(sheq (pm-locals-find-files-downwards default-directory nil)
		  (list "/tmp/subdir/.dir-locals.el"))

	(mkdir "/tmp/subdir/sub1/sub2/" t)
	(with-temp-buffer
	  (write-file "/tmp/subdir/sub1/sub2/.dir-locals.el"))

	(sheq (pm-locals-find-files-downwards default-directory nil)
		  (list "/tmp/subdir/.dir-locals.el"
				"/tmp/subdir/sub1/sub2/.dir-locals.el"))

	(sheq (pm-locals-find-files-downwards default-directory nil 1)
		  (list "/tmp/subdir/.dir-locals.el")))

  ;; caches
  (let ((default-directory (concat "/tmp/subdir")))
	(find-file-noselect "sub1/sub2/file.txt")

	(with-current-buffer "file.txt"
	  (pm-locals-set '((somevar . 42)) :category nil :persist t :sync-buffers t))
	(sheq somevar 42)
	)

  (sheq (pm-locals-find-caches-downwards "/tmp")
		(list "/tmp/" "/tmp/subdir/"))
  (sheq (pm-locals-find-caches-downwards "/tmp" pm-locals--main-vars-template)
		(list "/tmp/"))
  )

;; ---------------------------------------------------------
(ert-deftest pm-locals-delete ()
  (let ((default-directory "/tmp"))
	(ignore-errors
	  (f-delete ".dir-locals.el"))
	(pm-locals-create "/tmp/" (pm-locals--main-stash-create "/tmp/"))

	(sheq (f-exists? ".dir-locals.el") t)

	(pm-locals-delete-file default-directory)
	(sheq (f-exists? ".dir-locals.el") nil)
	(let-alist (pm-locals-read-cache default-directory)
	  (sheq .nil.projectile-project-root "/tmp/")
	  (sheq .nil.current-target nil))

	(pm-locals-delete-cache default-directory)
	(let-alist (pm-locals-read-cache default-directory)
	  (sheq .nil.projectile-project-root nil)
	  (sheq .nil.current-target nil)))
  )

;; ---------------------------------------------------------
(ert-deftest pm-locals-set ()

  (let ((default-directory (concat "/tmp")))

	;; ---- scenario ---------------------------
	;; cleanup
	(ierrs
	 (f-delete (concat ".dir-locals.el"))
	 (pm-locals-delete-file "/tmp/")
	 (pm-locals-delete-cache "/tmp")
	 (pm-locals-delete-file "/tmp/sub1")
	 (pm-locals-delete-cache "/tmp/sub1/")
	 (pm-locals-delete-file "/tmp/sub1/sub2/")
	 (pm-locals-delete-cache "/tmp/sub1/sub2/")
	 )
	(mapcar #'(lambda (f) (ignore-errors (kill-buffer f)))
			'("hsfile.hs" "file.txt" "hsfile1.hs" "file1.txt" "hsfile2.hs"
			  "file2.txt"))

	;; check clean
	(let-alist (pm-locals-read-cache "/tmp/")
	  (sheq .nil.projectile-project-root nil)
	  (sheq .nil.current-target nil)
	  (sheq .nil.test nil))
	(let-alist (pm-locals-read-cache "/tmp/sub1")
	  (sheq .nil.projectile-project-root nil)
	  (sheq .nil.current-target nil)
	  (sheq .nil.test nil))
	(let-alist (pm-locals-read-cache "/tmp/sub1/sub2/")
	  (sheq .nil.projectile-project-root nil)
	  (sheq .nil.current-target nil)
	  (sheq .nil.test nil))

	(should-error (pm-locals-set (list)))

	(remove-hook 'find-file-hook #'pm-locals-update-here)

	;; 	;; create:
	(mkdir "/tmp/sub1/sub2/" t)
	(pm-locals-create "/tmp/" (pm-locals--main-stash-create "/tmp/"))
	(find-file-noselect "/tmp/hsfile.hs")
	(find-file-noselect "/tmp/file.txt")
	(find-file-noselect "/tmp/sub1/hsfile1.hs")
	(find-file-noselect "/tmp/sub1/file1.txt")
	(find-file-noselect "/tmp/sub1/sub2/hsfile2.hs")
	(find-file-noselect "/tmp/sub1/sub2/file2.txt")

	;; top stash
	(let-alist (pm-locals-read-cache "/tmp/")
	  (sheq .nil.projectile-project-root "/tmp/"))

	;; ;; nothing set yet in files below
	(with-current-buffer (get-buffer "file2.txt")
	  (should-error current-target))

	;; bottom-up: open new file with hook
	(add-hook 'find-file-hook #'pm-locals-update-here)
	(kill-buffer "file2.txt")
	(find-file-noselect "/tmp/sub1/sub2/file2.txt")
	(with-current-buffer "file2.txt"
	  (sheq current-target nil))

	;; top-down:   update buffers from root
	(pm-locals-update-buffers "/tmp/")

	(with-current-buffer "file.txt"
	  (sheq projectile-project-root "/tmp/"))
	(with-current-buffer "file2.txt"
	  (sheq projectile-project-root "/tmp/"))

	;; scenario:  sub-dir-local in a subdir, should
	;;   1) shadow variables set in root
	;;   2) take up changes made below subdir
	(pm-locals-create "/tmp/sub1" (list (cons 'haskell-mode
											  (list (cons 'hsvar 20)))))
	(with-current-buffer "hsfile2.hs"
	  (should-error hsvar)
	  (pm-locals-update-here)
	  (sheq hsvar 20)

	  (pm-locals-set (list (cons 'hsvar2 30)))
	  (pm-locals-set (list (cons 'generalvar 40)) :category 'nil)

	  (should-error hsvar2)
	  (should-error generalvar)

	  (let-alist (pm-locals-read-cache "/tmp/sub1")
	  	(sheq .haskell-mode.hsvar 20)
	  	(sheq .haskell-mode.hsvar2 30)
	  	(sheq .nil.generalvar nil))

	  (let-alist (pm-locals-read-file "/tmp/sub1")
	  	(sheq .haskell-mode.hsvar 20)
	  	(sheq .haskell-mode.hsvar2 nil)
	  	(sheq .nil.generalvar nil))

	  (pm-locals-update-here)
	  (sheq hsvar2 30)
	  (sheq generalvar 40)

	  (pm-locals-set (list (cons 'hsvar2 60))
	  				 :persist t :sync-buffers t)
	  (let-alist (pm-locals-read-cache "/tmp/sub1")
	  	(sheq .haskell-mode.hsvar2 60))
	  (let-alist (pm-locals-read-file "/tmp/sub1")
	  	(sheq .haskell-mode.hsvar2 60))

	  (pm-locals-set (list (cons 'generalvar2 42))
	  				 :category nil :persist t :sync-buffers t)
	  (let-alist (pm-locals-read-cache "/tmp/")
	  	(sheq .nil.generalvar2 42))
	  (let-alist (pm-locals-read-file "/tmp")
	  	(sheq .nil.generalvar2 42))

	  (with-current-buffer "hsfile.hs"
		(should-error hsvar2)
		(pm-locals-set (list (cons 'hsvar2 "top-level-val"))
					   :sync-buffers t
					   :persist t)
		(sheq hsvar2 "top-level-val"))

	  (let-alist (pm-locals-read-file "/tmp/sub1")
	  	(sheq .haskell-mode.hsvar2 60))
	  (let-alist (pm-locals-read-file "/tmp/")
	  	(sheq .haskell-mode.hsvar2 "top-level-val"))

	  (with-current-buffer "hsfile2.hs"
		(pm-locals-set (list (cons 'hsvar2 "bottom-level-val"))
					   :sync-buffers t
					   :persist t)
		(sheq hsvar2 "bottom-level-val"))

	  (let-alist (pm-locals-read-file "/tmp/sub1")
	  	(sheq .haskell-mode.hsvar2 "bottom-level-val"))
	  (let-alist (pm-locals-read-file "/tmp/")
	  	(sheq .haskell-mode.hsvar2 "top-level-val")))))
