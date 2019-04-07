;; modules/ui/treemacs/config.el -*- lexical-binding: t; -*-

(message "treemacs/config.el ...")

(use-package treemacs
  :config
;;; theming
  (treemacs-resize-icons 14)
  (treemacs-git-mode 'deferred)

  (defun +theme--treemacs-mode-hook ()
	(setq header-line-format nil)
	(when (display-graphic-p)
	  (set-window-fringes nil 1 1))
	(setq-local mode-line-format nil)
	(when (not treemacs--width-is-locked)
	  (treemacs-toggle-fixed-width))
	)

  ;; hide root icon
  (plist-put (cdr (get-text-property 0 'display treemacs-icon-root-png)) :scale 0.1)
  (plist-put (cdr (get-text-property 0 'img-selected treemacs-icon-root-png)) :scale 0.1)
  (plist-put (cdr (get-text-property 0 'img-unselected treemacs-icon-root-png)) :scale 0.1)

  (advice-add #'treemacs-mode :after #'+theme--treemacs-mode-hook)

;;; automatic horizontal scrolling
  (setq +treemacs-scroll-right t)

  (defun +treemacs-next-line (f &optional count)
	(next-line count)
	(treemacs--evade-image)
	(when +treemacs-scroll-right
	  (evil-end-of-line))
	)

  (defun +treemacs-previous-line (f &optional count)
	(previous-line count)
	(treemacs--evade-image)
	(when +treemacs-scroll-right
	  (evil-end-of-line)))

  (advice-add #'treemacs-next-line :around #'+treemacs-next-line)
  (advice-add #'treemacs-previous-line :around #'+treemacs-previous-line)

  (defun +treemacs-scroll-right-on ()
	(interactive)
	(setq +treemacs-scroll-right t)
	(evil-end-of-line))

  (defun +treemacs-scroll-right-off ()
	(interactive)
	(setq +treemacs-scroll-right nil)
	(evil-beginning-of-line))

  (evil-define-key 'treemacs treemacs-mode-map (kbd "$") #'+treemacs-scroll-right-on)
  (evil-define-key 'treemacs treemacs-mode-map (kbd "0") #'+treemacs-scroll-right-off)

  ;; -- auto adjust after follow
  (defun +treemacs-after-goto-file-node (&rest args)
	(let ((align-right nil))
	  (save-excursion
		(when (> (move-to-column 1000) treemacs-width)
		  (setq align-right t)))
	  (when align-right
		(evil-end-of-line)
		(setq +treemacs-scroll-right t))))

  (advice-add #'treemacs-goto-file-node :after #'+treemacs-after-goto-file-node)

;;; keys
;;;; toggle
  (setq +treemacs--system-modules-dir (concat dir_emacs "modules"))

  (defun +treemacs-toggle ()
	"Toggles display of the treemacs sidebar, and ensures that a
treemacs-project is shown that best matches current context:
* projectile-root if it exists,
* the directory of the current file otherwise."
	(interactive)
	(let* ((current-window (selected-window))
		   (project-root (ignore-errors (f-full (projectile-project-root))))
		   (new-treemacs-root (f-full
							   (cond ((s-contains? +treemacs--system-modules-dir default-directory)
									  ;; in system/emacs/modules directory:  use that as treemacs-root
									  +treemacs--system-modules-dir)
									 (project-root
									  ;; projetile-root exists, use that as treemacs-root
									  project-root)
									 (t
									  ;; neither, use default-directory as treemacs-root
									  default-directory))))
		   (current-treemacs-root (ignore-errors
									(f-full (treemacs-project->path
											 (car (treemacs-workspace->projects (treemacs-current-workspace)))))))
		   (treemacs-window (treemacs-get-local-window))
		   (treemacs-buffer (treemacs-get-local-buffer)))
	  (when (and (null treemacs-window)
				 (not (string= project-root current-treemacs-root)))
		;; -- window not shown and treemacs project root needs to be changed
		(let ((treemacs-workspace (ignore-errors (treemacs-current-workspace))))
		  (if (not treemacs-workspace)
			  (progn
				;; workspace non-existant -> initialize
				(treemacs--find-workspace)
				(setf treemacs-workspace (treemacs-current-workspace)))
			;; workspace exists -> reset
			(progn
			  ;; remove projects
			  (cl-loop for project in (treemacs-workspace->projects (treemacs-current-workspace))
					   do
					   (treemacs-do-remove-project-from-workspace project)
					   (treemacs--remove-project-from-current-workspace project))
			  ;; reset buffer
			  (when treemacs-buffer
				(kill-buffer treemacs-buffer)))))
		;; -- add new project
		(treemacs-do-add-project-to-workspace new-treemacs-root (f-short new-treemacs-root)))
	  ;; call treemacs, keep focus in current window
	  (treemacs)
	  (select-window current-window)))

  (spacemacs/set-leader-keys "ft" #'+treemacs-toggle)

  ;; keys:
  ;; hl on directories:  open/close
  ;; jk up/down

  (defun +treemacs-open-node (&optional arg)
	(interactive)
	(treemacs-do-for-button-state
	 ;; :on-root-node-open (treemacs--collapse-root-node btn arg)
	 :on-root-node-closed (treemacs--expand-root-node btn)
	 :on-dir-node-closed (treemacs--expand-dir-node btn :recursive arg)
	 :on-file-node-closed (treemacs--expand-file-node btn arg)
	 :on-tag-node-closed (treemacs--expand-tag-node btn arg)
	 :on-tag-node-leaf (progn (other-window 1) (treemacs--goto-tag btn))))

  (defun +treemacs-close-node (&optional arg)
	(interactive)
	(treemacs-do-for-button-state
	 :on-root-node-open (treemacs--collapse-root-node btn arg)
	 :on-dir-node-open (treemacs--collapse-dir-node btn arg)
	 :on-file-node-open (treemacs--collapse-file-node btn arg)
	 :on-tag-node-open (treemacs--collapse-tag-node btn arg)))

  (define-key evil-treemacs-state-map (kbd "h") #'+treemacs-close-node)
  (define-key evil-treemacs-state-map (kbd "l") #'+treemacs-open-node)
  (evil-define-key 'treemacs treemacs-mode-map (kbd "h") #'+treemacs-close-node)
  (evil-define-key 'treemacs treemacs-mode-map (kbd "l") #'+treemacs-open-node)
  (evil-define-key 'treemacs treemacs-mode-map (kbd "+") #'treemacs-create-dir)

  (evil-define-key 'treemacs treemacs-mode-map (kbd "M-h") #'treemacs-goto-parent-node)
  (evil-define-key 'treemacs treemacs-mode-map (kbd "H") #'treemacs-root-up)
  (evil-define-key 'treemacs treemacs-mode-map (kbd "L") #'treemacs-root-down)

  (defun +treemacs-visit-node-no-split (f &optional arg)
	"Like treemacs-visit-node-no-split but uses the
most-recently-used window within the current frame to open the
file in."
	(interactive "P")
	(treemacs--execute-button-action
	 :file-action (find-file (treemacs-safe-button-get btn :path))
	 :dir-action (dired (treemacs-safe-button-get btn :path))
	 :tag-section-action (treemacs--visit-or-expand/collapse-tag-node btn arg nil)
	 :tag-action (treemacs--goto-tag btn)
	 :save-window arg
	 :ensure-window-split t
	 :window (get-last-mru-window-among (window-list))
	 :no-match-explanation "Node is neither a file, a directory or a tag - nothing to do here."))

  (advice-add #'treemacs-visit-node-no-split :around #'+treemacs-visit-node-no-split)


  )
