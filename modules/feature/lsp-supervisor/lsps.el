;;; lsps.el                                         -*- lexical-binding: t -*-
;;
;; Filename: lsps.el
;;
;; Copyright (C) 2018 Florian Thevißen
;;
;; Description: Emacs language server supervision
;; Author: Florian Thevißen <mail@florian-thevissen.de>
;; Keywords: lisp, tools
;; Created: Sun Dec 25 20:35:00 2018 (+0100)
;; Version: 0.1.0
;; Package-Required: TODO
;; URL: TODO
;;
;; -----------------------------------------------------------------------------
;;; Commentary
;;
;;  Support library for supervision of code completion libraries.
;;
;; -----------------------------------------------------------------------------
(message "loading lsps ...")

;; =========================================================
;; == public interface

(defun lsps-enable-lsp ()  (interactive) (lsps--enable-disable 'lsp t))
(defun lsps-disable-lsp () (interactive) (lsps--enable-disable 'lsp nil))
(defun lsps-enable-ycmd () (interactive) (lsps--enable-disable 'ycmd t))
(defun lsps-disable-ycmd ()(interactive) (lsps--enable-disable 'ycmd nil))


(defun lsps--enable-disable (tool-sym enable)
  "Collect and verify information prior to passing it to
  `lsps--do-enable' or `lsps--do-disable'."

  ;; NOTE: for now assume that
  ;;   - PJSON, LSP and PROJECTILE all point to the same directory.
  ;;     There may be dragons if they're not.
  ;;   - every project is a pjson project (...-> change for python projects?)

  (let ((project-root (projectile-project-root))
		(project-sym (intern (pjson-current-project))))
	(if enable
		(lsps--do-enable tool-sym project-sym project-root)
	  (lsps--do-disable tool-sym project-sym project-root))))

(defvar lsps-enabled-tools-changed-hooks '()
  "Hooks run after, for any project, its tools changed e.g.
  enabled/disabled.")

;; =========================================================
;; == handlers
(cl-defun lsps--do-enable (tool-sym project project-root)
  "
tool:      symbol
project:   symbol
project-root: string
"
  ;; tool already enabled?
  (when (lsps--projects->tools-enabled-p project tool-sym)
	(error (format "Tool %s already enabled for project %s"
				   (symbol-name tool-sym) (symbol-name project))))

  ;; another tool already enabled? -> disable it
  (let ((other-tools (lsps--projects->other-enabled-tools project tool-sym)))
	(cl-loop for ot in other-tools do
			 (lsps--disable ot project)))

  (let ((tool (map-elt lsps--tools tool-sym)))
	;; run pre-enable-pred-fn that checks if enabling the tool is viable
	(let* ((pred-fun (lsps--tool-pre-enable-pred-fn tool))
		   (checks-passed (if pred-fun
							  (progn
								(message "calling..?")
								(funcall pred-fun project-root))
							t			; no predicate function -> passed
							)))
	  (if (not checks-passed)
		  (message "Cannot enable tool %s for %s, predicate checks failed." (lsps--tool-name tool) project-root)
		(progn
		  (message "Enabling tool %s for %s, predicate checks passed." (lsps--tool-name tool) project-root)
		  ;; enable tool for project
		  (when-let ((fun (lsps--tool-enable-fn-project tool)))
			(funcall fun project-root))

		  ;; update dir-locals for project-root
		  (+utils-set-file-local-variable (lsps--tool-dir-local-enabled-name tool)
										  t
										  project-root)

		  ;; update suitable buffers (file locals + run tool's local enable defun)
		  (+utils-with-every-live-buffer-below
		   project-root
		   (progn
			 (when (or (null (lsps--tool-major-modes tool))
					   (member major-mode (lsps--tool-major-modes tool)))
			   (hack-local-variables)
			   (funcall (lsps--tool-enable-fn-local tool))) ))))))

  ;; add tool to lsps--projects->tools
  (lsps--projects->tools-add project tool-sym)

  (run-hooks 'lsps-enabled-tools-changed-hooks))

(cl-defun lsps--do-disable (tool-sym project project-root)
  (when (not (lsps--projects->tools-enabled-p project tool-sym))
	(error (format "Tool %s already disabled for project %s"
				   (symbol-name tool-sym) (symbol-name project))))

  (let ((tool (map-elt lsps--tools tool-sym)))
	;; update dir-locals for project root
	(+utils-set-file-local-variable (lsps--tool-dir-local-enabled-name tool)
									nil
									project-root)
	;; update buffers (file locals = run tool's local disable defun
	(+utils-with-every-live-buffer-below
	 project-root
	 (progn
	   (when (or (null (lsps--tool-major-modes tool))
				 (member major-mode (lsps--tool-major-modes tool)))
		 (hack-local-variables)
		 (funcall (lsps--tool-disable-fn-local tool)))))

	;; disable tool for project
	(when-let ((fun (lsps--tool-disable-fn-project tool)))
	  (funcall fun project-root))
	)

  ;; remove tool from lsps--projects->tools
  (lsps--projects->tools-remove project tool-sym)

  (run-hooks 'lsps-enabled-tools-changed-hooks))

(defun lsps--on-open-new-file ()
  "Hook to be run after a new file is opened, but after
  file-local variables have been set."
  ;; enable tool (minor-mode), if it is enabled
  ;; NOTE: take care of major-modes in lsps--do-X functions.
  (dolist (tool lsps--tools)
	(when-let* ((tool-sym (cdr tool))
				(local-variable (lsps--tool-dir-local-enabled-name tool-sym))
				(exists (boundp local-variable))
				(set (eval local-variable)))
	  (funcall (lsps--tool-enable-fn-local tool-sym)))))

;; TODO: called twice... why?
(add-hook 'hack-local-variables-hook #'lsps--on-open-new-file)

(defun lsps--on-lsps-enabled-tools-changed ()
  "Run "
  (dolist (tool lsps--tools)
	(when-let* ((tool (cdr tool))
				(fun (lsps--tool-all-projects-closed-fn tool))
				(tool-not-in-use (eq 0 (lsps--number-of-projects-with
										(lsps--tool-name tool)))  ))
	  (funcall fun)
	  )
	)
  )

(add-hook #'lsps-enabled-tools-changed-hooks #'lsps--on-lsps-enabled-tools-changed)

;; =========================================================
;; == global state

(defvar lsps--projects->tools '()
  "Indicates what tools are enabled in what project. Form:
  '( ('proj-symbol . ('tool1 'tool2 'tool3) ) ")

(defun lsps--projects->tools-add (project tool-sym)
  (lsps--projects->tools-remove project tool-sym)
  (let ((enabled-tools (map-elt lsps--projects->tools project)))
	(map-put lsps--projects->tools project (push tool-sym enabled-tools))))

(defun lsps--projects->tools-remove (project tool-sym)
  (let ((enabled-tools (map-elt lsps--projects->tools project)))
	(map-put lsps--projects->tools project (remove tool-sym enabled-tools))))

(defun lsps--projects->tools-enabled-p (project tool-sym)
  (-contains? (map-elt lsps--projects->tools project) tool-sym))

(defun lsps--projects->other-enabled-tools (project tool-sym)
  (when (seq-difference (map-elt lsps--projects->tools project) `(,tool-sym))))

(defun lsps--number-of-projects-with (tool-sym)
  (-count (lambda (enabled-tools) (member tool-sym enabled-tools))
		  (map-values lsps--projects->tools)))

;; TODO: maybe do away with this state, just keep it all in dir-locals

;; =========================================================
;; tool management

(defstruct lsps--tool
  (name nil)					 ; symbol
  (dir-local-enabled-name nil)	 ; symbol
  (major-modes '())				 ; list of symbols, major-modes for which loaded
  (enable-fn-local)				 ; function to enable minor-mode in buf
  (disable-fn-local)			 ; function to disable -"-
  (enable-fn-project)            ; function to enable tool for project, optionally takes project-root as arg
  (disable-fn-project)           ; function to disable tool for project, optionally takes project-root as arg
  (all-projects-closed-fn)	     ; function to run when no projects active anymore
  (pre-enable-pred-fn)			 ; function run before project is enabled, to return t if loading to commence, optionally takes project-root
  )

(defvar lsps--tools '()
  "Alist representing registered tools. Has the form ('tool-name
lsps--tool-instance).")

(defun lsps-register-tool (tool)
  (map-put lsps--tools (lsps--tool-name tool) tool))

(defun lsps--tool-get (tool)
  (map-elt lsps--tools tool))

;; ---------------------------------
;; tools

;; =========================================================
;; == TODO: persistence  (how? wenn überhaupt?)

;; optionally, keep projects with tool X open permanently, no matter what.
;; also, especially, on system startup to give time for lsp to pre-cache


;; == lsp monitoring =====================================
(defun lsps--num-running-processes (&optional lang)
  "Return number of processes for language lang."
  )

(defun lsps--folders-in-workspace ()
  (lsp-session-folders (lsp-session)))

(defun lsps--start-client (project-root)

  )

(defun lsps--shutdown-client (project-root)
  ;; -> use lsp-shutdown-workspace?
  (lsp)

  (if (-contains? (lsp-session-folders (lsp-session)) project-root)
	  (kill-process (lsp--workspace-proc ()))
	(kill-process (lsp--workspace-proc (car (lsp--session-workspaces (lsp-session)))))
	)
  )

(defun lsps--start-client (project-root)

  )

(defun lsps-enable-if ()
  "Enables "
  )



;; (lsp--workspace-proc (cadr (lsp--session-workspaces (lsp-session))))
;; (lsp--workspace-root (cadr (lsp--session-workspaces (lsp-session))))
;; (lsp--workspace-status (cadr (lsp--session-workspaces (lsp-session))))

;; lsp-workspace-folders-switch
;; lsp-workspace-folders-remove
;; lsp-workspace-folders-add

;; ---------------------------------------------------------
;; ycmd

;; (ycmd-running-p)
;; ...can't start/stop on my own though I think.


;; ---------------------------------------------------------
;;

(defun lsps-project-has-compilation-database (root)
  nil

  )



(provide 'feature/lsp-supervisor/lsps)

