;;; modules/feature/pm/pm-cc-tool.el                   -*- lexical-binding: t; -*-

(require 'cl-generic)

;;;;; pm--cc-tool
(cl-defstruct pm--cc-tool
  "A code-comprehension tool for any language and pm-target-type."
  (name nil)					 ; symbol
  (dir-local-enabled-name nil)	 ; symbol
  (major-modes '())				 ; list of symbols, major-modes for which loaded
  (enable-fn-local)				 ; function to enable minor-mode in buf
  (disable-fn-local)			 ; function to disable -"-
  (enable-fn-project) ; function to enable tool for project, optionally takes project-root as arg
  (disable-fn-project) ; function to disable tool for project, optionally takes project-root as arg
  (all-projects-closed-fn)	   ; function to run when no projects active anymore
  (pre-enable-pred-fn) ; function run before project is enabled, to return t if loading to commence, optionally takes project-root
  )



;; (cl-defun pm-target-create (type)
;;   "Creates new target of TYPE. Handles generic pm-target things
;;   like TODO (interactive directory-creation if necessary?), and
;;   then dispatches to the constructor of TYPE."
;;   (pcase type
;; 	('c++ (pm-target-c++-create))
;; 	(_ (error "Cannot create pm-target for unknown type %s" type))
;; 	)
;;   )


