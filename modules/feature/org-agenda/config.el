;; modules/feature/org-agenda/config.el    -*- lexical-binding: t; -*-

(use-package org-agenda
  :after (org)

  :init
  (spacemacs/set-leader-keys (kbd "aa") 'fn-org-agenda)

  :config
  (+load "org-agenda-tools.el")

  (evil-define-key 'normal org-mode-map "t" nil)
  (evil-define-key 'normal org-mode-map
	(kbd "tt") '(lambda () (interactive) (org-todo "TODO"))
	(kbd "td") '(lambda () (interactive) (org-todo "DONE"))
	(kbd "tw") '(lambda () (interactive) (org-todo "WAITING"))
	(kbd "tc") '(lambda () (interactive) (org-todo "CANCELED"))
	)

  ;; TODO: necessary?
  ;; (run-with-timer 5 nil #'(lambda ()
  (define-key org-agenda-keymap "t" nil)
  (define-key org-agenda-mode-map "t" nil)
  (evil-define-key '(normal visual evilified) org-agenda-mode-map
	(kbd "tt") '(lambda () (interactive) (org-agenda-todo "TODO"))
	(kbd "td") '(lambda () (interactive) (org-agenda-todo "DONE"))
	(kbd "tw") '(lambda () (interactive) (org-agenda-todo "WAITING"))
	(kbd "tc") '(lambda () (interactive) (org-agenda-todo "CANCELED"))
	(kbd "{")  '(lambda () (interactive) (evil-backward-paragraph))
	(kbd "}")  '(lambda () (interactive) (evil-forward-paragraph))
	(kbd "C-n") 'org-agenda-next-item
	(kbd "C-p") 'org-agenda-previous-item
	(kbd "C-j") 'org-agenda-next-item
	(kbd "C-k") 'org-agenda-previous-item
	(kbd "gb") #'acc-ido-switch-buffer)
				  ;; )


  (define-key org-agenda-keymap (kbd "C-x C-f") #'find-file)
  (advice-add #'org-agenda :around #'fn-suppress-delete-other-windows)

  (setq location (getenv "LOC"))

  (setq org-agenda-prefix-format '((agenda . " %i %?-12t% s")
								   (todo . " %i %(concat \"[\"(org-format-outline-path (org-get-outline-path))\"]  \")" )
								   (tags . " %i ")
								   (search . " %i ")))

  ;; TODO: necessary...?
  (setq org-agenda-custom-commands
		(quote
		 (("n" "Agenda and all TODOs"
		   ((agenda "" nil)
			(alltodo "" nil))
		   nil)
		  ("p" "priority" tags-todo "+PRIORITY=\"2\"|+PRIORITY=\"1\"" nil)
		  ("h" "home"
		   ((tags "PRIORITY=\"1\""
				  (
				   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
				   (org-agenda-overriding-header "High-priority unfinished tasks:")))
			;; (agenda ""
			;; 		(
			;; 		 (org-agenda-ndays 1)))
			(alltodo ""
					 (
					  (org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
													 (air-org-skip-subtree-if-priority ?1)
													 (org-agenda-skip-if nil '(scheduled deadline))))
					  (org-agenda-overriding-header "ALL normal priority tasks:"))))
		   ((org-agenda-compact-blocks nil)))
		  ("w" "work"
		   ((tags "PRIORITY=\"A\""
				  ((org-agenda-files (list (concat dir_org "/work.org")))
				   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
				   (org-agenda-overriding-header "High-priority unfinished tasks:")))
			(agenda ""
					((org-agenda-files (list (concat dir_org "/work.org")))
					 (org-agenda-ndays 1)))
			(alltodo ""
					 ((org-agenda-files (list (concat dir_org "/work.org")))
					  (org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
													 (air-org-skip-subtree-if-priority ?A)
													 (org-agenda-skip-if nil '(scheduled deadline))))
					  (org-agenda-overriding-header "ALL normal priority tasks:"))))
		   ((org-agenda-compact-blocks nil)))
		  ("d" "day" agenda ""
		   ((org-agenda-sorting-strategy
			 (quote
			  (agenda time-up priority-down tag-up)))
			(org-deadline-warning-days 0)))
		  ("o" "overview"
		   ((agenda "" ((org-agenda-span 1)))
			(tags-todo "+PRIORITY=\"A\"")
			(todo "WAITING")
			)
		   ((org-agenda-compact-blocks nil)))
		  ("u" "tags-tree" tags-tree "t") ;; tags-tree search
		  ("c" "category" tags-todo (concat "+CATEGORY=\"" cats "\"") nil)
		  )))

  ;; define "R" as the prefix key for reviewing what happened in various
  ;; time periods
  (add-to-list 'org-agenda-custom-commands
			   '("R" . "Review" )
			   )

  ;; ;; Common settings for all reviews
  ;; (setq efs/org-agenda-review-settings
  ;; 		'(
  ;; 		  (org-agenda-files (list (concat dir_org "/home.org")
  ;; 								  (concat dir_org "/work.org")))
  ;; 		  (org-agenda-show-all-dates t)
  ;; 		  (org-agenda-start-with-log-mode t)
  ;; 		  (org-agenda-start-with-clockreport-mode t)
  ;; 		  (org-agenda-archives-mode t)
  ;; 		  (org-agenda-compact-blocks nil)
  ;; 		  ;; I don't care if an entry was archived
  ;; 		  (org-agenda-hide-tags-regexp
  ;; 		   (concat org-agenda-hide-tags-regexp
  ;; 				   "\\|ARCHIVE"))
  ;; 		  ))

  ;; Show the agenda with the log turn on, the clock table show and
  ;; archived entries shown.  These commands are all the same exept for
  ;; the time period.
  ;; (add-to-list 'org-agenda-custom-commands
  ;; 			   `("Rw" "Week in review"
  ;; 				 agenda ""
  ;; 				 ;; agenda settings
  ;; 				 ,(append
  ;; 				   efs/org-agenda-review-settings
  ;; 				   '((org-agenda-span 'week)
  ;; 					 (org-agenda-start-on-weekday 0)
  ;; 					 (org-agenda-overriding-header "Week in Review"))
  ;; 				   )
  ;; 				 ;; ("~/org/review/week.html")
  ;; 				 ))


  ;; (add-to-list 'org-agenda-custom-commands
  ;; 			   `("Rd" "Day in review"
  ;; 				 agenda ""
  ;; 				 ;; agenda settings
  ;; 				 ,(append
  ;; 				   efs/org-agenda-review-settings
  ;; 				   '((org-agenda-span 'day)
  ;; 					 (org-agenda-overriding-header "Week in Review"))
  ;; 				   )
  ;; 				 ;; ("~/org/review/day.html")
  ;; 				 ))

  ;; (add-to-list 'org-agenda-custom-commands
  ;; 			   `("Rm" "Month in review"
  ;; 				 agenda ""
  ;; 				 ;; agenda settings
  ;; 				 ,(append
  ;; 				   efs/org-agenda-review-settings
  ;; 				   '((org-agenda-span 'month)
  ;; 					 (org-agenda-start-day "01")
  ;; 					 (org-read-date-prefer-future nil)
  ;; 					 (org-agenda-overriding-header "Month in Review"))
  ;; 				   )
  ;; 				 ;; ("~/org/review/month.html")
  ;; 				 )) )
  )

(use-package org-super-agenda
  :after (org-agenda porg)

  :bind (:map org-agenda-keymap
  			  ("C-S-l" . fn-org-agenda--inc-priority)
			  ("C-S-h" . fn-org-agenda--dec-priority)
			  )
  :config

  ;; ----
  ;; TODO: fix, I think it just broke, recently
  ;; overwrite to remove the "Category: " prefix, and (TODO) fontify headers
  (defun fn-org-super-agenda--auto-group-category (all-items &rest ignore)
	"Divide ALL-ITEMS into groups based on their org-category property."
	(cl-loop with categories = (ht-create)
			 for item in all-items
			 for category = (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
							  (org-get-category))
			 if category
			 do (ht-set! categories category (cons item (ht-get categories category)))
			 else collect item into non-matching
			 finally return (list :auto-category
								  non-matching
								  (cl-loop for key in (sort (ht-keys categories) #'string<)
										   for name = key
										   collect (list :name name
														 :items (ht-get categories key))))))
  (advice-remove #'org-super-agenda--auto-group-category #'fn-org-super-agenda--auto-group-category)
  ;; (advice-add #'org-super-agenda--auto-group-category :override #'fn-org-super-agenda--auto-group-category)
  ;; ----

  )
