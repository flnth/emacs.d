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

