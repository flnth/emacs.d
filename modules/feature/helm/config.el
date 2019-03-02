;; modules/feature/helm/config.el    -*- lexical-binding: t; -*-


;;; helm
(use-package helm
  :commands (helm-mode helm-mini)
  :bind (("<menu>" . 'helm-mini)
		 :map helm-map
		 ("<tab>" . 'helm-execute-persistent-action)
		 ("C-i"   . 'helm-execute-persistent-action) ; make TAB work in tty
		 ("C-z"   . 'helm-select-action)             ; list actions
		 ("C-d"   . 'helm-next-page)
		 ("C-u"   . 'helm-previous-page))
  )

;;; helm-projectile
(use-package helm-projectile
  :after (helm projectile)
  :bind (("<C-menu>"  . 'helm-projectile)
		 ("<M-menu>"  . 'helm-projectile-find-file-in-known-projects)
		 ("<S-menu>"  . 'helm-projectile-switch-project))
  )


;;; helm-org-rifle
(use-package helm-org-rifle
  :after (helm org)
  :commands (fn-helm-org-rifle-occur-goto-entry helm-org-rifle)
  :init
  (evil-define-key 'normal helm-org-rifle-occur-map (kbd "RET") 'fn-helm-org-rifle-occur-goto-entry)
  ;; (evil-define-key 'normal helm-org-rifle-occur-map (kbd "RET") 'helm-org-rifle-occur-goto-entry)
  (evil-define-key 'normal helm-org-rifle-occur-map (kbd "q") 'helm-org-rifle-occur-cleanup-buffer)

  :config
  (defun fn-helm-org-rifle-occur-goto-entry ()
	"Go to node in source buffer that point in occur buffer is in.
Same as original except that 1) this always opens in same window
2) point relative to entry top before same as after."
	(interactive)
	(-let* ((properties (text-properties-at (point)))
			((&plist :buffer buffer :node-beg node-beg) properties)
			;; FIXME: Get offset of point in node (not sure why +2 is necessary but it works)...or does it?
			(offset (+ 2 (- (point) (or (previous-single-property-change (point) :node-beg)
										(point-min))))))
	  (when node-beg
		(let* ((screen-line (fn-point-to-screen-line (point)))
			   (point-target (+ node-beg offset)))
		  (switch-to-buffer buffer)
		  (goto-char point-target)
		  (org-show-entry)
		  (fn-position-point-in-window screen-line point-target)))))
  )
