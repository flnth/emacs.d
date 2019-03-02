;;; modules/feature/pm/tests/test-pm.el     -*- lexical-binding: t; -*-

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

(defun test-pm--create-environment (&rest files)
  (pm-locals-create default-directory (pm-locals--main-stash-create default-directory))
  (-map (lambda (fname)
		  (find-file-noselect (concat (f-slash default-directory) fname)))
		files))

(defun test-pm--clean-environment (&rest files)
  ;; cleanup
  (ierrs
   (f-delete (concat ".dir-locals.el"))
   (pm-locals-delete-file default-directory)
   (pm-locals-delete-cache default-directory)
   )
  (mapcar #'(lambda (f) (ignore-errors (kill-buffer f)))
		  files)

  ;; check clean
  (let-alist (pm-locals-read-cache default-directory)
	(sheq .nil.projectile-project-root nil)
	(sheq .nil.targets nil)
	(sheq .nil.current-target nil)
	(sheq .nil.test nil))
  )

;; =========================================================

(ert-deftest pm-create ()
  (let ((default-directory (concat "/tmp")))

	;; -- clean, create ------------------------
	(test-pm--clean-environment "file.cpp")
	;; (test-pm--create-environment "file.cpp")
	;; -----------------------------------------

	(find-file-noselect "/tmp/file.cpp")
	(with-current-buffer "file.cpp"

	  (pm-create :name "testname"
				 :root "/tmp"
				 :meta-dir "/tmp")

	  (sheq project-name "testname")
	  (sheq (f-slash projectile-project-root) (f-slash "/tmp"))
	  (sheq project-meta-dir "/tmp"))))


(ert-deftest pm--targets ()

  (let ((default-directory (concat "/tmp")))

	;; -- clean, create ------------------------
	(test-pm--clean-environment "file.cpp")
	(test-pm--create-environment "file.cpp")
	;; -----------------------------------------

	(with-current-buffer "file.cpp"
	  (sheq (pm--targets) nil))

	(pm-locals-set (list (cons 'targets (list (cons 'target1 (list (cons 'type 'c++)))
											  (cons 'target2 (list (cons 'type 'c++)))
											  (cons 'target3 (list (cons 'type 'c++))))))
				   :category nil
				   :persist t
				   :sync-buffers t)

	(with-current-buffer "file.cpp"
	  (sheq (pm--targets) (list 'target1 'target2 'target3))
	  (sheq (pm--targets :strings t) (list "target1" "target2" "target3"))
	  (sheq (pm--targets :sorted t) (list 'target1 'target2 'target3))

	  (pm-locals-set (list (cons 'current-target 'target3))
					 :category nil
					 :sync-buffers t)
	  (sheq current-target 'target3)

	  ;; NOTE: no recency sorting yet
	  (sheq (pm--targets :sorted t) (list 'target3 'target1 'target2))
	  (sheq (pm--targets :strings t :sorted t) (list "target3" "target1" "target2"))

	  (sheq (pm--targets :alist t) `(("target1" . target1)
									 ("target2" . target2)
									 ("target3" . target3)))
	  (sheq (pm--targets :alist t :strings t) (pm--targets :alist t))
	  (sheq (pm--targets :alist t :sorted t) `(("target3" . target3)
											   ("target1" . target1)
											   ("target2" . target2)))

	  (pm-locals-set `((current-target . target4))
					 :category nil
					 :sync-buffers t)
	  (sheq current-target 'target4)
	  (sheq (pm--current-target) nil)
	  (sheq current-target nil))

	;; (sheq (pm--targets t))

	;; create target
	;; compile:  test necessary helper functions
	;;   - list of targets
	;;   - list of targets ordered by recency (--> pcache? =))
	;;   - list of target strings, including types, ordered

	;; (pm-locals-set (list ('current-target .  ) ))

	;; target selection/filtering/helper methods
	;; (sheq   )
	)
  )





