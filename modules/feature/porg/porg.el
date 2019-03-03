;;; porg.el                           -*- lexical-binding: t; -*-


;;; backend
;; set json file
(let* ((current-filename (or load-file-name
							 (buffer-file-name)))

	   (current-dir (file-name-directory current-filename))
	   (json-path (concat dir_emacs "share/" "porg." (system-name) ".json")))
  (when (not (f-exists? json-path))
	(with-temp-buffer
	  (insert "{}")
	  (write-file json-path)))
  (setq porg--json (config-json-create :path json-path)))

(defun porg--json-get! (&optional force-reload)
  (if force-reload
	  (setf (config-json-last-mod porg--json) 0))
  (setf porg--json (config-json-load porg--json)))

(defun porg-json-synchronize ()
  (interactive)
  (message "porg-json-synchronize:  synchronizing...")
  (let ((loaded-agenda-files (length porg--agenda-files)))
	;; local + file -> file
	(porg--update-json-from-agenda-files! porg--json porg--agenda-files)
	(config-json-synchronize! porg--json)
	;; file -> local
	(porg--json-get!)
	(setf porg--agenda-files (porg--agenda-files-from-json porg--json))
	;; clear cache (cached file could've been removed)
	(setf porg--agenda-file-cache nil)
	(setf porg--agenda-files (porg--agenda-files-sort porg--agenda-files))
	(message "  ..done. Loaded %s new agenda files. Total: %s"
			 (- (length porg--agenda-files) loaded-agenda-files)
			 (length porg--agenda-files)))
  )

;;;; data structures

(defvar porg--agenda-files nil
  "List of current porg--agenda-file instances, ordered.")

(defun porg--last-modified (path)
  (cadr (nth 5 (file-attributes path))))

(defstruct (porg--agenda-file
			(:type vector)
			(:constructor
			 porg--agenda-file-create (identifier path project &aux (last-modified (porg--last-modified path))))
			:named
			(:copier nil))
  "Internal representation of an porg agenda file."
  identifier							; convention: filename base
  path
  project								; project-name (-- pjson?)
  last-modified							; last-modified
  )

(defun porg--agenda-files-from-json (json)
  "Construct new UNSORTED list of porg agenda-files from JSON instance."
  (setq j json)
  (loop for (identifier . content) in (config-json-contents json)
		if (f-exists? (asoc-get content 'path))
		collect (progn
				  (setq s content)
				  (porg--agenda-file-create identifier
											(asoc-get content 'path)
											(asoc-get content 'project)))
		else do (message "--porg--agenda-files-from-json:  Warning: agenda file %s does not exist, skipping..."
						 (asoc-get content 'path))))

(defun porg--update-json-from-agenda-files! (json agenda-files)
  "update config-json instance JSON with new content from
AGENDA-FILES."
  (setf (config-json-contents json)
		(loop for f in agenda-files
			  if (f-exists? (porg--agenda-file-path f))
			  collect `(,(porg--agenda-file-identifier f) .
						((path . ,(porg--agenda-file-path f) )
						 (project . ,(porg--agenda-file-project f))))
			  else do
			  (message "--porg--update-json-from-agenda-files: Warning: agenda file %s does not exist, skipping..."
					   (porg--agenda-file-path f)))))

;;;; accessors, cache, traversal
(defvar porg--agenda-file-cache nil)

(defun porg-get-matching-agenda-file (symbol el)
  "Returns first agenda file whose SYMBOL matches the string EL.
A target is either a project-name, or the identifier."
  (when (not (null el))
	(let ((accessor-fun (pcase symbol
						  ('identifier 'porg--agenda-file-identifier)
						  ('path 'porg--agenda-file-path)
						  ('project 'porg--agenda-file-project)
						  ('target #'(lambda (f) (let ((project (porg--agenda-file-project f)))
											  (if project project
												(porg--agenda-file-identifier f))))))))
	  (loop for f in (if (not (null porg--agenda-file-cache))
						 (cons porg--agenda-file-cache porg--agenda-files)
					   porg--agenda-files)
			when (string= el (apply accessor-fun `(,f)))
			return (setq porg--agenda-file-cache f)))))

(defun porg--agenda-get-file-from-context ()
  "Returns the agenda-file associated with current context.
Context atm entirely specified by projectile.
NOTE: hard-dependency on pjson for project-name atm."
  (porg-get-matching-agenda-file 'project
								 (pjson-current-project)))
;;;;; helpers

(defun porg--get-previous-in-lst (el lst)
  "Get the element before el that is in lst.
If el is first, return el. If el is not in lst, return nil."
  (if (and lst (cdr lst))
	  (let ((predicate 'equal)
			(current (car lst))
			(next (cadr lst)))
		(if (or (equal el current)
				(equal el next))
			current
		  (porg--get-previous-in-lst el (cdr lst))))))

(defun porg--get-next-in-lst (el lst)
  "Get the element after el.
If el is last, return el. If el is not in lst, return nil."
  (if lst
	  (if (equal el (car lst))
		  (if (cadr lst)
			  (cadr lst)
			el)
		(porg--get-next-in-lst el (cdr lst)))))

;;;;; next/previous/cycling
(defun porg--agenda-get-previous (fpath-or-afile)
  (let ((agenda-file (if (stringp fpath-or-afile)
						 (porg-get-matching-agenda-file 'path fpath)
					   fpath-or-afile)))
	(porg--get-previous-in-lst agenda-file porg--agenda-files)))

(defun porg--agenda-get-next (fpath-or-afile)
  (let ((agenda-file (if (stringp fpath-or-afile)
						 (porg-get-matching-agenda-file 'path fpath)
					   fpath-or-afile)))
	(porg--get-next-in-lst agenda-file porg--agenda-files)))

(cl-defun porg-cycle (&optional prev &key force-cache)
  "Cycle through `porg--agenda-files'.
If force-cache is nil, and not in `porg--agenda-files' buffer,
try to infer one from context and go there. Otherwise go to
`porg--agenda-file-cache'"
  (interactive)
  (let* ((fun (if prev 'porg--agenda-get-previous
				'porg--agenda-get-next))
		 (fpath (buffer-file-name))
		 (agenda-file (porg-get-matching-agenda-file 'path fpath)))
	(if (null agenda-file)				; TODO:  factor out commonalities in following two branches
		(progn
		  ;; switch into agenda
		  (cond
		   (force-cache (setf agenda-file (cond
										   (porg--agenda-file-cache)
										   (t (setf porg--agenda-file-cache (car porg--agenda-files))))))
		   ((setf agenda-file (porg--agenda-get-file-from-context)))
		   ((setf agenda-file porg--agenda-file-cache))
		   ((setf agenda-file (car porg--agenda-files)))
		   (t (message "--porg-cycle--: Warning: no agenda-files loaded. Skipping... ")))
		  (when (not (null agenda-file))
			(porg--hist-add!)			; NOTE: history
			(let* ((fpath (porg--agenda-file-path agenda-file))
				   (filename (f-filename fpath))
				   (buf (get-buffer filename )))
			  (if buf
				  (switch-to-buffer (f-filename (porg--agenda-file-path agenda-file)) nil t)
				(find-file fpath) ; TODO:  enforce switch-to-buffer behaviour here
				))
			))
	  (progn
		;; cycle within agenda
		(let ((next-agenda-file (funcall fun agenda-file)))
		  (setq aa next-agenda-file)
		  (when (not (equal next-agenda-file agenda-file))
			(let* ((fpath (porg--agenda-file-path next-agenda-file))
				   (filename (f-filename fpath))
				   (buf (get-buffer filename )))
			  (if buf
				  (switch-to-buffer (f-filename (porg--agenda-file-path next-agenda-file)) nil t)
				(find-file fpath) ; TODO:  enforce switch-to-buffer behaviour here
				))))))))

;;;; modifying functions
(defun porg--agenda-files-sort (agenda-files &optional force-check-modified)
  "Sort agenda files according to 1) modified and 2) other
  hardcoded specs (TODO). Side-effect force check modified times
  on force-check-modified t."
  (let ((agenda-files-copy (copy-tree agenda-files)))
	(when force-check-modified
	  (loop for f in agenda-files-copy
			if (f-exists? (porg--agenda-file-path f))
			do (setf (porg--agenda-file-last-modified f)
					 (porg--last-modified (porg--agenda-file-path f)))))
	;; sort by last-modified
	(sort agenda-files-copy
		  #'(lambda (afile1 afile2)
			  (< (porg--agenda-file-last-modified afile2)
				 (porg--agenda-file-last-modified afile1)))))
  ;; TODO: sort by other criteria (i.e. force some buffers to be at the front or end
  )

(defun porg--move-project-agenda-file-front (fpath-or-afile)
  "Move agenda file or fpath to front of `porg--agenda-files'."
  ;; precondition:  fpath exists, fpath is org file, file is tracked
  (let ((agenda-file (if (stringp fpath-or-afile)
						 (porg-get-matching-agenda-file 'path fpath)
					   fpath-or-afile)))
	(if agenda-file
		(setq porg--agenda-files (cons agenda-file (remove agenda-file porg--agenda-files)))
	  (message "--porg--move-project-agenda-file-front:  Warning: file %s not valid, skipping..." fpath-or-afile))))

;;;; adding / removal

(defun porg-add-agenda-file (&optional fpath)
  "Adds the file FPATH to the porg tracked agenda files, and
  synchronizes the config-json file.

Ignores fpath if it is already being tracked. Uses the file of
the current buffer if fpath is not set. NOTE: hard-dependency on
pjson."
  (interactive)
  (let ((fpath (if fpath fpath (buffer-file-name))))
	(cond
	 ((not (s-suffix? "org" fpath))
	  (message "--porg-add-agenda-file:  Warning: %s is not an org file, skipping..." fpath))
	 ((porg-get-matching-agenda-file 'path fpath)
	  (message "--porg-add-agenda-file:  Warning: file %s already being tracked, skipping..." fpath))
	 (t
	  (let ((pjson-project (let ((current-pjson-project (pjson-current-project)))
							 (if (and current-pjson-project
									  (y-or-n-p (format "pjson project context found: %s. Associate with agenda file? "
														current-pjson-project)))
								 current-pjson-project
							   nil))))
		(when (and (not pjson-project)
				   (y-or-n-p (format "Select a pjson project to associate with agenda file? ")))
		  (setf pjson-project (ivy-read "pjson-project> "
										(pjson-known-projects))))
		;; save file, create agenda file, store, update cache and json file
		(save-buffer)
		(setf porg--agenda-files (cons (porg--agenda-file-create (intern (file-name-base fpath))
																 fpath
																 pjson-project)
									   porg--agenda-files))
		(setf porg--agenda-file-cache (car porg--agenda-files))
		(porg-json-synchronize))))))

(defun porg-remove-agenda-file (&optional fpath)
  "Removes the file FPATH from the porg tracked agenda files, and
  synchronizes the config-json file."
  (interactive)
  (let* ((fpath (if fpath fpath (buffer-file-name)))
		 (agenda-file (porg-get-matching-agenda-file 'path fpath)))
	(cond
	 ((not (s-suffix? "org" fpath))
	  (message "--porg-remove-agenda-file:  Warning: %s is not an org file, skipping..." fpath))
	 ((null agenda-file)
	  (message "--porg-remove-agenda-file:  Warning: %s is not being tracked, cannot remove, skipping..." fpath))
	 (t
	  (setq porg--agenda-files (remove agenda-file porg--agenda-files))
	  (porg-json-synchronize)))))

;;;; hooks,history

(defun porg--on-agenda-file-save ()
  "Move the file saved to front if it is a tracked porg agenda
file. Update last-modified slot."
  (let* ((fpath (buffer-file-name))
		 (agenda-file (if (s-suffix? "org" fpath)
						  (porg-get-matching-agenda-file 'path fpath)
						nil)))
	(when agenda-file
	  (setf (porg--agenda-file-last-modified agenda-file) (nth 1 (current-time)))
	  (porg--move-project-agenda-file-front agenda-file))))

(add-hook 'before-save-hook 'porg--on-agenda-file-save)

;;;; history

(defvar porg--per-window-entry-history '()
  "Per-window history storing where focus was before entering
  porg.")

(defun porg--hist-add! ()
  "Adds current-frame, window and buffer to history."
  ;; exists? -> update
  (let ((found nil)
		(sel-win (selected-window))
		(cur-buf (current-buffer)))
	(loop for el in porg--per-window-entry-history
		  when (eq (car el) sel-win)
		  return (progn
				   (setf found t)
				   (setf (cdr el) cur-buf)))
	;; not in there? -> add
	(when (not found)
	  (setf porg--per-window-entry-history (cons `(,sel-win . ,cur-buf)
												 porg--per-window-entry-history)))))

(defun porg--hist-remove (&optional win)
  "Removes window WIN from history."
  (let ((win (if win win (selected-window))))
	(setf porg--per-window-entry-history
		  (asoc--filter (not (eq key win)) porg--per-window-entry-history))))

(advice-add 'delete-window :after 'porg--hist-remove)


;;;; querying information

;; TODO: cache.
(defun porg-get-target-strings ()
  "Create a list of strings containing either project- or, if
there is no project-association, identifiers.."
  (interactive)
  (loop for f in porg--agenda-files collect
		(let ((project (porg--agenda-file-project f)))
		  (if project project
			(symbol-name (porg--agenda-file-identifier f))))))

;;; frontend

;;;; minibuffer

(defun porg-switch ()
  "Change current buffer."
  (interactive)
  (ivy-read "porg-file|target> " (porg-get-target-strings)
			:action #'(lambda (sel) (interactive)
						(switch-to-buffer (let* ((afile (porg-get-matching-agenda-file 'target sel))
												 (fpath (porg--agenda-file-path afile))
												 (buf (get-buffer (f-filename fpath))))
											(if buf buf (find-file fpath)))))))

(defun porg-refile ()
  "Wrapper around org-refile. Prompt for project (TODO: regex),
then refile into that."
  (interactive)
  (ivy-read "porg-refile|target> " (porg-get-target-strings)
			:action #'(lambda (sel) (interactive)
						(let* ((afile (porg-get-matching-agenda-file 'target sel))
							   (fpath (porg--agenda-file-path afile))
							   ;; (org-refile-use-outline-path t)
							   (org-refile-targets `((,fpath :maxlevel . 2)))
							   )
						  (org-refile)
						  ))))


;;;; global C-S-hjkl navigation

;; NOTE: temporary
(defun porg--C-S-hl-handler (&optional prev)
  "Upon entering porg, mark where we came from.
TODO: If we entered porg before in this window, go back to where we
were before."
  (interactive)
  ;; TODO:  on C-S-j,  get from history (-> store where we were before in porg)
  ;;        on C-S-hl, get from context
  (when (not (porg-get-matching-agenda-file 'path (buffer-file-name)))
	;; porg-cycle will enter -> push history
	(porg--hist-add!))
  (porg-cycle prev :force-cache nil))

;;;;; lateral movement
(global-set-key (kbd "C-S-h") #'(lambda () (interactive) (porg--C-S-hl-handler t)))
(global-set-key (kbd "C-S-l") 'porg--C-S-hl-handler)

(evil-define-key '(insert normal visual) evil-org-mode-map (kbd "C-S-h") #'(lambda () (interactive) (porg--C-S-hl-handler t)) )
(evil-define-key '(insert normal visual) evil-org-mode-map (kbd "C-S-l") 'porg--C-S-hl-handler )

(evil-define-key '(insert normal visual) org-mode-map (kbd "C-S-h") #'(lambda () (interactive) (porg--C-S-hl-handler t)) )
(evil-define-key '(insert normal visual) org-mode-map (kbd "C-S-l") 'porg--C-S-hl-handler )

;;;;; vertical movement
(defun porg--C-S-j-handler ()
  "Descend into porg, if not already there."
  (interactive)
  (when (not (porg-get-matching-agenda-file 'path (buffer-file-name)))
	;; porg-cycle will enter -> push history
	(porg--hist-add!)
	(porg-cycle :force-cache t)))

(defun porg--C-S-k-handler ()
  "Get out of porg if inside back to where we were before."
  (interactive)
  (when (porg-get-matching-agenda-file 'path (buffer-file-name))
	(let ((buf (asoc-get porg--per-window-entry-history (selected-window))))
	  (when buf
		(switch-to-buffer buf nil t)))))

;; (global-set-key (kbd "C-S-j") #'(lambda () (interactive) (porg-cycle nil :force-cache t)) )
;; (global-set-key (kbd "C-S-k") 'porg--C-S-k-handler )

;; (evil-define-key '(insert normal visual) evil-org-mode-map (kbd "C-S-j") #'(lambda () (interactive) (porg-cycle nil :force-cache t)) )
;; (evil-define-key '(insert normal visual) evil-org-mode-map (kbd "C-S-k") 'porg--C-S-k-handler )

;; (evil-define-key '(insert normal visual) org-mode-map (kbd "C-S-j") #'(lambda () (interactive) (porg-cycle nil :force-cache t)) )
;; (evil-define-key '(insert normal visual) org-mode-map (kbd "C-S-k") 'porg--C-S-k-handler )

;;;; agenda

(defmacro porg--with-org-agenda-files (&rest body)
  `(let ((org-agenda-files (loop for f in porg--agenda-files collect
								 (porg--agenda-file-path f)) ))
	 ,@body))

;;; init

(porg-json-synchronize)

(provide 'feature/porg/porg)
