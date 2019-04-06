;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

(setq server-socket-dir "/tmp/emacs1000/")

(setq spacemacsdir (concat (getenv "DIR_EMACSD") "spacemacs/" ))
(message (concat "spacemacsdir: " spacemacsdir))
(setq default-directory spacemacsdir)
(setq user-emacs-directory spacemacsdir)

(setq gc-cons-threshold 100000000)

(setq exec-path-from-shell-arguments '("-c"))

(add-hook 'window-setup-hook 'delete-other-frames)

(let ((file-name-handler-alist nil))
  (load-file (concat spacemacsdir "init.el"))
  )
(put 'timer-list 'disabled nil)
(put 'list-timers 'disabled nil)
