;;; modules/feature/pm/type-c++.el                   -*- lexical-binding: t; -*-

(cl-defstruct (pm-target-c++
			   (:include pm-target)
			   (:constructor nil)
			   (:constructor pm-target-c++--create))
  "Represents a language-specific target type. Encapsulates
  information, and offers type-independent interface for
	 - run/build-management
		. how to run/build a target from a command?
	 - code completion tools
     - directory management:
        - how to determine,create source-/build-paths
        - e.g. for C++ default to out-of-source builds in
          'meta-dirs'
     - sub .dir-locals.el management:
        - specify target specifics in pm .dir-locals.el
        - give target the option to install its own
          .dir-locals.el e.g. in one (or more) source-dirs
        - triggered by build/run/changing targets

  All target-specific tasks are launched from here using
  dynamic dispatch."
  ;; (type nil			:type symbol)		; target knows its type (typed union)
  ;; (source-dir ""	:type string)
  ;; (build-dir  ""	:type string)
  )

(cl-defun pm-target-c++-create (sname)
  (pm-target-c++--create :type 'c++ :name (symbol-name sname)))

(cl-defmethod pm-target-run ((_ pm-target-c++) )
  (message "pm-target-run for c++ types"))

(cl-defmethod pm-target-compile ((_ pm-target-c++))
  (message "pm-target-compile for c++ types"))

(cl-defmethod pm-target-enable-code-completion ((_ pm-target-c++))
  (message "pm-target-enable-code-completion for c++ types"))

(cl-defmethod pm-target--as-string ((target pm-target))
  (pm-target-name target))

(provide 'pm-target-c++)
