;; modules/app/elfeed/config.el    -*- lexical-binding: t; -*-

;;; elfeed

(use-package elfeed
  :commands (elfeed)

  :init
  (spacemacs/set-leader-keys "af" 'elfeed)

  :config
  (setq rmh-elfeed-org-files (list (concat (getenv "DIR_ORG") "/.rss.org")))

  (defun fn-elfeed-from-term ()
	(set-frame-parameter nil 'elfeed-frame t)
	(elfeed))

  (defun fn-quit-elfeed ()
	"If elfeed was entered directly from terminal, close frame.
Otherwise do a normal exit."
	(interactive)
	(quit-window)
	(when (eq (frame-parameter nil 'elfeed-frame) t)
	  (delete-frame)))

  (evilified-state-evilify-map elfeed-search-mode-map
	:mode elfeed-search-mode
	:eval-after-load elfeed-search
	:bindings
	"c"  'elfeed-db-compact
	"gr" 'elfeed-update
	"gR" 'elfeed-search-update--force
	"gu" 'elfeed-unjam
	"o"  'elfeed-load-opml
	"q"  'fn-quit-elfeed
	"gb" 'ivy-switch-buffer)

  (evilified-state-evilify-map elfeed-show-mode-map
	:mode elfeed-show-mode
	:eval-after-load elfeed-show
	:bindings
	"q" 'quit-window
	(kbd "C-j") 'elfeed-show-next
	(kbd "C-k") 'elfeed-show-prev)

  (evil-define-key 'visual elfeed-search-mode-map
	"+"  'elfeed-search-tag-all
	"-"  'elfeed-search-untag-all
	"b"  'elfeed-search-browse-url
	"y"  'elfeed-search-yank
	(kbd "<RET>") 'fn-elfeed-search-show-entry)
  )

;;; * stuff
;; (elfeed/init-elfeed)
;; (elfeed/init-elfeed-goodies)
;; (elfeed/init-elfeed-org)

;; reload all elfeed modes
;; (defun reload-all-elfeed-modes ()
;;   (dolist ($buf (buffer-list (current-buffer)))
;;     (with-current-buffer $buf
;;       (when (equal major-mode 'elfeed-search-mode) (elfeed-search-mode) )
;;       (when (equal major-mode 'elfeed-show-mode) (elfeed-show-mode) )
;;       )))
;; (reload-all-elfeed-modes)

;; ;;functions to support syncing .elfeed between machines
;; ;;makes sure elfeed reads index from disk before launching
;; (defun bjm/elfeed-load-db-and-open ()
;;   "Wrapper to load the elfeed db from disk before opening"
;;   (interactive)
;;   (elfeed-db-load)
;;   (elfeed)
;;   (elfeed-search-update--force))

;; ;;write to disk when quiting
;; (defun bjm/elfeed-save-db-and-bury ()
;;   "Wrapper to save the elfeed db to disk before burying buffer"
;;   (interactive)
;;   (elfeed-db-save)
;;   (quit-window))

;; ;;shortcut functions
;; (defun bjm/elfeed-show-all ()
;;   (interactive)
;;   (bookmark-maybe-load-default-file)
;;   (bookmark-jump "elfeed-all"))
;; (defun bjm/elfeed-show-emacs ()
;;   (interactive)
;;   (bookmark-maybe-load-default-file)
;;   (bookmark-jump "elfeed-emacs"))
;; (defun bjm/elfeed-show-daily ()
;;   (interactive)
;;   (bookmark-maybe-load-default-file)
;;   (bookmark-jump "elfeed-daily"))

