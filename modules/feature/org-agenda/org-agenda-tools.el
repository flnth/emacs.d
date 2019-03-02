;; modules/feature/org-agenda/org-agenda-tools.el    -*- lexical-binding: t; -*-


;; -- helper functions for composite agenda view -------------------------------
(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.
  PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
  		(pri-value (* 1000 (- org-lowest-priority priority)))
  		(pri-current (org-get-priority (thing-at-point 'line t))))
  	(if (= pri-value pri-current)
  		subtree-end
  	  nil)))

(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
  	(if (string= (org-entry-get nil "STYLE") "habit")
  		subtree-end
  	  nil)))

(setq fn-org-agenda--priority 2)
(defun fn-org-agenda ()
  (interactive)
  (porg--with-org-agenda-files
   (let ((org-agenda-prefix-format
	 	  '((agenda . " %i %?-12t% s")
	 		(todo . " %i %(concat \"[\"(org-format-outline-path (org-get-outline-path))\"]  \")" )
	 		(tags . " %i ")
	 		(search . " %i "))) ; TODO: necessary in here atm. fix that (-> org-agenda-prefix-format overwritten somewhere)
	 	 (org-agenda-custom-commands
	 	  '(("u" "Super view"
	 		 ((tags (concat "+PRIORITY<=\"" (number-to-string fn-org-agenda--priority) "\"")
	 				((org-agenda-overriding-header "Projects")
	 				 (org-agenda-sorting-strategy '(priority-up))
	 				 (org-super-agenda-groups
	 				  '((:discard (:scheduled future))
	 					(:auto-category)
	 					(:discard (:anything t)))))))))))
	 (org-agenda nil "u")
	 )
   ;; (org-agenda nil "u")
   ))

(defun fn-org-agenda--inc-priority ()
  (interactive)
  (setq fn-org-agenda--priority
		(min 2 (+ fn-org-agenda--priority 1)))
  (fn-org-agenda))

(defun fn-org-agenda--dec-priority ()
  (interactive)
  (setq fn-org-agenda--priority
		(max 1 (- fn-org-agenda--priority 1)))
  (fn-org-agenda))
