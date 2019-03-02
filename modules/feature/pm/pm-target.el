;;; modules/feature/pm/pm-target.el                   -*- lexical-binding: t; -*-

(require 'cl-generic)

(cl-defstruct (pm-target
			   (:constructor nil)
			   (:constructor pm-target--create (&key type name)))
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
  dynamic dispatch. Targets know how to navigate pm-locals."

  (type nil			:type symbol) ; target knows its type (typed union) TODO: really necessary...?
  (name nil         :type string) ; target knows its name (= key in pm-locals targets)
  ;; (source-dir ""	:type string)
  ;; (build-dir  ""	:type string)
  )

;;;; public interface

(cl-defun pm-target-create (sname &optional stype)
  "Creates new target of symbol STYPE and symbol SNAME. If STYPE
is not given, it is looked up in TARGETS using SNAME. Handles
generic pm-target things like TODO (interactive
directory-creation if necessary?), and then dispatches to the
constructor of STYPE."

  ;; TODO: maybe provide an abstraction here; dispatch on previously-registered
  ;; stypes.

  ;; NOTE: maybe also introduce some form of caching here, to prevent having to
  ;; re-construct the same thing on every use of it e.g. from recompilation
  ;;   -> need hook on .dir-locals.el edits to invalidate, then.
  ;;      (want that hook anyways, though)

  (when (null stype)
	(setf stype
		  (map-nested-elt targets (list sname 'type))))

  (pcase stype
	('c++ (pm-target-c++-create sname))
	(_ (error "Cannot create pm-target for unknown stype %s" stype))))

;;;;; actions
(cl-defmethod pm-target-run ((_ pm-target))
  "Runs the target as specified by the target's run-command, and
the way this type of target runs commands in general."
  (error "Not implemented in pm-target base."))

(cl-defmethod pm-target-compile ((_ pm-target))
  "Compiles the target as specified by the target's compile command,
and the way this type of target compiles things in general."
  (error "Not implemented in pm-target base.."))

(cl-defmethod pm-target-enable-code-completion ((_ pm-target))
  "Enables code completion for this target. May offer a selection
of tools if there is more than one available, or may offer a
default e.g. based on a configuration somewhere."
  (error "Not implemented in pm-target base."))

;;;;; data access
(cl-defmethod pm-target--as-string ((_ pm-target))
  "Returns a stringified representation of itself."
  (error "Not implemented in pm-target base."))


;; (cl-defun pm-type-register (&key pm-type )

;;   )

;; (cl-defun pm-type-create-target (&key pm-type) )

(provide 'pm-target)

