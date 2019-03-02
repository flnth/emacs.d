;; modules/feature/org/config.el    -*- lexical-binding: t; -*-


;; bullet symbols: ●•◦·►
;; ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
;;  ;; ► • ★ ▸‣▪▫
;; ✸‣
;; ⁃-⁌⁍∙◘•

(defun +to-warnings-buffer (str)
  (with-current-buffer "*Warnings*"
	(let ((inhibit-read-only t))
	  (goto-char (point-max))
	  (insert (concat str "\n")))))


;; ---------------------------------------------------------
(defun +org--config-capture ()
  (setq location (getenv "LOC"))

  (setq org-refile-targets
		(quote
		 ((org-agenda-files :maxlevel . 2)
		  (nil :maxlevel . 2))))

  (defun fn--porg-capture-get-olp ()
	(let* ((target (completing-read "target> " (porg-get-target-strings) nil t))
		   (porg-afile (porg-get-matching-agenda-file 'target target))
		   (m (org-find-olp `(,(porg--agenda-file-path porg-afile) "Tasks"))))
	  (set-buffer (marker-buffer m))
	  (org-capture-put-target-region-and-position)
	  (widen)
	  (goto-char m)
	  (set-marker m nil)))

  (setq org-capture-templates
		(quote (("p" "porg" entry (function fn--porg-capture-get-olp) "* TODO %?\n"))))

  (global-set-key (kbd "C-c c") #'(lambda () (interactive)
									(org-capture nil "p")) )


  ;; reload all org-buffers, because not all features seem to be loaded at the
  ;; beginning, whyever that is...
  ;; TODO:  still necessary?
  (reload-all-org-mode-buffers)

  ;; -- calendar sync
  ;; (setq org-caldav-url "http://81.169.247.85/nextcloud/remote.php/dav/calendars/admin/personal/")
  ;; (setq org-caldav-calendar-id "")

  ;; (setq org-caldav-inbox "/tmp/cal.org")
  ;; (setq org-caldav-files '("/tmp/cal-tracked.org"))

  ;; (setq org-icalendar-timezone "Europe/Berlin")
  ;; (setq org-icalendar-date-time-format ";TZID=%Z:%Y%m%dT%H%M%S")

  )

(use-package org
  :commands (org-mode)

  ;; :bind (:map org-mode-map
  ;; 			  ("M-RET" . smething))

  :init 								; -----------------------
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
	(kbd "r") 'helm-org-rifle-occur-current-buffer
	(kbd "ns") 'org-narrow-to-subtree
	)

  :config 								; -----------------------
  (load "feature/porg/config")
  (+load "org-tools.el")

  (setq dir_system (getenv "DIR_SYSTEM"))

  ;; -- fixes
  ;; TODO: still necessary?
  (defun ob-ipython-auto-configure-kernels (&optional replace))

  (defun org-fancy-priorities-wrap (f &rest args)
	(ignore-errors
	  (apply f args)))
  (advice-add #'org-fancy-priorities-mode :around #'org-fancy-priorities-wrap)
  ;; --------
  (add-hook 'kill-emacs-hook 'org-save-all-org-buffers)

  (global-set-key (kbd "<\eRET>") 'org-meta-return) ; TODO:  these necessary as global? org-mode only?
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "M-n") 'org-next-visible-heading)
  (global-set-key (kbd "M-p") 'org-previous-visible-heading)
  ;; use default bury function for log-buffers for tabbar comp atibility

  (define-key magit-log-mode-map "q" #'magit-mode-bury-buffer) ; TODO:  -> magit?

  (evil-define-key '(normal insert visual) org-mode-map
	(kbd "M-RET") #'+org-insert-heading
	(kbd "M-n") #'org-next-visible-heading
	(kbd "M-p") #'org-previous-visible-heading)

  (evil-define-key '(normal) org-mode-map
	(kbd ",r") 'org-rifle-occur-current-buffer
	)

  (advice-add 'org-cycle-hide-drawers :around 'fn--around-org-cycle-hide-drawers)
  (evil-define-key '(normal visual) org-mode-map
	(kbd "M-<tab>") #'fn-org-cycle)

  (evil-define-key 'normal evil-org-mode-map
	(kbd "M-,") #'org-promote-subtree
	(kbd "M-.") #'org-demote-subtree
	)

  (evil-define-key '('normal,'insert,'visual,'replace,'operator,'motion,'emacs) evil-org-mode-map
	(kbd "M-h") nil
	(kbd "M-j") nil
	(kbd "M-k") nil
	(kbd "M-l") nil)

  (evil-define-key 'normal org-mode-map (kbd "gm") 'counsel-org-goto)

  ;; ----------
  ;; TODO: necessary?
  ;; (defun on-org-mode-load ()
  ;; 	(define-key org-mode-map (kbd "M-j") nil)
  ;; 	(define-key org-mode-map (kbd "M-h") nil)
  ;; 	(define-key org-mode-map (kbd "M-k") nil)
  ;; 	(define-key org-mode-map (kbd "M-l") nil)
  ;; 	(local-set-key "\M-j" nil))
  ;; (progn
  ;; 	(add-hook 'org-mode-hook 'on-org-mode-load)
  ;; 	(define-key org-mode-map (kbd "M-j") nil)
  ;; 	(define-key org-mode-map (kbd "M-h") nil)
  ;; 	(define-key org-mode-map (kbd "M-k") nil)
  ;; 	(define-key org-mode-map (kbd "M-l") nil)
  ;; 	)
  ;; -----------


  ;; around org-refile
  (advice-add #'org-refile :around #'fn-org-refile)

  ;; -------------------------------------------------------
  (add-hook 'org-add-hook 'font-lock-add-keywords)

  (defface org-waiting-face
	'((t (:foreground "#d7af87" :weight bold)))
	"Font used for WAITING keyword"
	:group 'org-faces )

  ;; dates
  (require 'german-holidays)
  (setq calendar-holidays holiday-german-NW-holidays)

  ;; prettification / formatting
  (setq org-startup-indented t
		org-pretty-entities t
		org-pretty-entities-include-sub-superscripts nil
		org-ellipsis " ▾"
		org-html-head "<style>* {background-color: black; }"
		org-html-head "<style type='text/css'>
	   /*<![CDATA[*/
		 p { font-weight: normal; color: gray; }
		 h1 { color: black; }
		 .title { text-align: center; }
		 .todo, .timestamp-kwd { color: red; }
		 .done { color: green; }
	   /*]]>*/
	  </style> "
		org-src-tab-acts-natively t
		org-hide-emphasis-markers t
		org-fontify-done-headline t
		;; scale in-line latex fragments  (TODO: couple this to a buffer's font size)
		org-format-latex-options (plist-put org-format-latex-options
											:scale 2.0)
		;; TODO:  centering latex equation fragments
		;; http://kitchingroup.cheme.cmu.edu/blog/2016/11/06/Justifying-LaTeX-preview-fragments-in-org-mode/
		)

  ;; unicode bullets http://www.howardism.org/Technical/Emacs/orgmode-wordprocessor.html
  (font-lock-add-keywords 'org-mode '(
									  ("^ +\\([-]\\) "
									   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "∙"))))))

  ;; priorities
  (require 'org-fancy-priorities)
  (defface org-priority-face-one
	'((t (:foreground "#ff0000")))
	"Face used to highlight priority 1 in org-mode.")
  (defface org-priority-face-two
	'((t (:foreground "#ffff00")))
	"Face used to highlight priority 1 in org-mode.")
  (setq org-fancy-priorities-list '((?1 . "")
									(?2 . "■")))
  (add-hook 'org-mode-hook 'org-fancy-priorities-mode)

  ;; editing --
  ;;  fix evil-repeat
  (mapc 'evil-declare-ignore-repeat
        '(org-end-of-line))

  ;; features ---
  ;; org-screenshot
  (load-file (concat dir_system "/emacs/org-attach-screenshot.el"))
  (setq org-attach-screenshot-command-line "mycommand -x -y -z %f")	; modify command for taking the screenshot
  (setq org-attach-screenshot-dirfunction ; modify the function for generating a directory name (!)
        (lambda ()				; check out org-attach-screenshot-relative-links
          (progn (assert (buffer-file-name))
                 (concat (file-name-sans-extension (buffer-file-name))
                         "_att"))))

  ;; helm-org-rifle
  (setq helm-org-rifle-occur-separator
		"―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――\n")
  (add-text-properties 0
					   (length helm-org-rifle-occur-separator)
					   '(font-lock-face helm-org-rifle-separator helm-org-rifle-result-separator t)
					   helm-org-rifle-occur-separator)

  ;; code-archive / library link
  ;; [[archive:cpp/iterators/170120_iterator_example1.cpp]]
  (setq dir_archive (getenv "DIR_CODEARCHIVE"))
  (setq org-link-abbrev-alist
		(list (cons "archive" (concat dir_archive "/%s"))))

  ;; -- google calendar org sync (org module)

  ;; (require 'org-gcal)
  ;; (setq org-gcal-client-id "399871993368-esplmgntuu09hep8b477airquj3j8agu.apps.googleusercontent.com"
  ;;       org-gcal-client-secret "3vj8lg2XxmTe3fUv"
  ;;       org-gcal-file-alist (list '("f.thevissen%40gmx.de" . "~/calendar.org")))
  ;; counsel-org-goto for org-mode


  ;; -- org-mode  stuff ----------------------------
  ;; (org-babel-do-load-languages
  ;;  'org-babel-load-languages
  ;;  '((C . t))
  ;;  )
  ;; (setq org-src-fontify-natively t)


  ;; (require 'ob-ipython)
  ;; (org-babel-do-load-languages
  ;;  'org-babel-load-languages
  ;;  '((emacs-lisp . t)
  ;;    (C . t)
  ;;    (ditaa . t)
  ;;    (dot . t)
  ;;    (js . t)
  ;;    (latex . t)
  ;;    (perl . t)
  ;;    (python . t)
  ;;    (ruby . t)
  ;;    (sh . t)
  ;;    (plantuml . t)
  ;;    (clojure . t)
  ;;    (ipython . t)
  ;;    ))
  ;; (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

  ;; -- window management

  ;; TODO: use, what?
  ;; (advice-add #'org-add-log-note :around #'doom*suppress-delete-other-windows)
  (advice-add #'org-capture-place-template :around #'fn-suppress-delete-other-windows)
  ;; (advice-remove #'org-capture-place-template #'fn-suppress-delete-other-windows)
  ;; (advice-add #'org-export--dispatch-ui :around #'doom*suppress-delete-other-windows)

  ;; Don't monopolize frame!

  (advice-add #'org-switch-to-buffer-other-window :override #'fn--org-window-handler)

  (+org--config-capture))

(use-package org-tempo
  :after org)

