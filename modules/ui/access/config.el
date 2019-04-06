;;; modules/ui/access/config.el                   -*- lexical-binding: t; -*-

(message "access/config.el ...")

(use-package ui/access/access
  ;; :after (ivy ido counsel projectile counsel-projectile f magit)
  :demand
  :bind (("M-x" . acc-M-x)
		 ("M-X" . counsel-M-x)
		 :map evil-motion-state-map
		 ("gb" . acc-ido-switch-buffer)
		 ("M-x" . acc-M-x)
		 ("M-X" . counsel-M-x)
		 :map messages-buffer-mode-map
		 ("gb" . acc-ido-switch-buffer)
		 )
  :commands (acc-counsel-projectile-rg-handler
			 acc-counsel-projectile-rg
			 acc-counsel-projectile-rg-query
			 acc-counsel-projectile-rg-subdir-downwards-handler
			 acc-counsel-from-subdir-downwards-rq-query
			 acc-counsel-from-subdir-downwards-rg
			 acc-ido-projectile-find-file
			 acc-ido-find-file-in-subdir-downwards
			 acc-fasd-find
			 acc-find-project
			 acc-counsel-projectile-switch-project
			 acc-counsel-projectile-magit
			 +access-repl-run-python
			 fn-toggle-repl-window
			 +access-eshell-toggle
			 )

  :init
  (use-package pcache
	:load-path "packages/pcache")
  (require 'window-purpose)
  (require 'window-purpose-switch)
  (require 'feature/compilation/pjson)
  (require 'ivy)
  (require 'ido)
  (require 'counsel)
  (require 'projectile)
  (require 'counsel-projectile)
  (require 'f)
  (require 'magit)
  (require 'helpful)
  (require 'ert)
  (require 'eshell)

  (spacemacs/set-leader-keys
	"ps" 'acc-counsel-projectile-rg-handler
	"pS" 'acc-counsel-projectile-rg-subdir-downwards-handler
	;; "pf" 'acc-ido-projectile-find-file
	"pf" 'acc-counsel-projectile-find-file
	"pF" 'acc-ido-find-file-in-subdir-downwards
	"pp" #'acc-counsel-projectile-switch-project
	"pr" #'acc-projectile-replace
	"p SPC" #'acc-counsel-projectile-magit
	"zd" #'(lambda () (interactive) (acc-fasd-find 'dir))
	"zf" #'(lambda () (interactive) (acc-fasd-find 'file))
	;; "zp" #'(lambda () (interactive) (acc-find-project)) ; NOTE: not implemented yet
	"gm" 'man)

  (evil-define-key '(normal visual emacs motion emacs evilified) special-mode-map (kbd "gb") #'acc-ido-switch-buffer )

  :config
  (run-with-timer
   5 nil (lambda ()
		   (setq counsel-rg-base-command (concat "rg -i --hidden --line-number --no-heading --color never "
												 (let ((ignore-file (concat dir_stackroot "etc/ripgrep_ignore")))
												   (when (f-exists? ignore-file)
													 (concat "--ignore-file " ignore-file " ")))
												 "%s ."))))

;;;; viewing pdfs from cloud (-> module access!)
)

(defun fn-view-library-pdf-right-and-focus ()
  (interactive)
  ;; TODO: does a window on the right exist?  (what constitutes a window on the right?)
  ;; if it does, use that
  ;; for now: just open a new one to the right
  (split-window-right-and-focus)
  (find-file (concat dir_library "/.projectile"))
  (helm-projectile-find-file)
  )

(defun fn-view-library-pdf ()
  (interactive)
  (find-file (concat dir_library "/.projectile"))
  (helm-projectile-find-file)
 )
;;;; searching code archive from cloud (-> module access!)

(setq dir_archive (getenv "DIR_CODEARCHIVE"))

(defun fn-search-code-archive ()
  (interactive)
  (find-file (concat dir_archive "/.projectile"))
  (helm-projectile-find-file)
  )

;;;; recently-used files (access)

(defun fn-parse-recently-used-files ()
  (with-temp-buffer
	(insert-file-contents "~/.local/share/recently-used.xbel")
	(libxml-parse-xml-region (buffer-end -1) (buffer-end 1))))


;; extract the last 10 elements

(defun fn-get-last-n-recent-files (&optional n)
  (let* ((n (if (null n) 10 n))
		 (ruf-parsed
		  (nthcdr 2
				  (with-temp-buffer
					(insert-file-contents "~/.local/share/recently-used.xbel")
					(libxml-parse-xml-region (buffer-end -1) (buffer-end 1)))))
		 (last-n-elements
		  (nthcdr (- (length ruf-parsed) n) ruf-parsed))
		 (last-n-paths
		  (loop for el in last-n-elements
				collect (alist-get 'href (cadr el)) )))
	last-n-paths))

(defun fn-insert-recent-file-path ()
  (interactive)
  (insert (car  (fn-get-last-n-recent-files 1)))
  )
;; search
(global-set-key (kbd "C-s") (lambda () (interactive)
							  (swiper (thing-at-point 'symbol))))

;; ivy minibuffer and occur stuff
(setq ivy-minibuffer-map (delq (assoc 9 ivy-minibuffer-map) ivy-minibuffer-map))
(define-key ivy-minibuffer-map (kbd "C-c C-c") 'ivy-call )
(define-key ivy-minibuffer-map (kbd "TAB") 'ivy-call )
(define-key ivy-occur-mode-map (kbd "<RET>") 'ivy-occur-press)

(defun fn-ivy-scroll-half-down ()
  "Scroll the candidates downward by the minibuffer height."
  (interactive)
  (ivy-set-index (max (1+ (- ivy--index (/ ivy-height 2)))
                      0)))

(defun fn-ivy-scroll-half-up ()
  "Scroll the candidates upward by the minibuffer height."
  (interactive)
  (ivy-set-index (min (1- (+ ivy--index (/ ivy-height 2)))
                      (1- ivy--length))))

(define-key ivy-minibuffer-map (kbd "C-u") 'fn-ivy-scroll-half-down)
(define-key ivy-minibuffer-map (kbd "C-d") 'fn-ivy-scroll-half-up)
;;;;; cursor movement

(require 'vertigo)
(define-key evil-motion-state-map (kbd "gj") 'vertigo-visible-jump-down )
(define-key evil-motion-state-map (kbd "gk") 'vertigo-visible-jump-up )
;; -> ibuffer:  access/ibuffer

(evil-define-key 'normal ibuffer-mode-map
  (kbd "/") 'ibuffer-jump-to-buffer
  (kbd "S") 'ibuffer-do-save			; TODO: doesnt work, fix
  )
;;;; accessing buffers (help,repls,etc)

;; -> module access

;; spacemacs:  major-mode's SPC-mhh binding used for K ("evil-smart-lookup")
(spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "hh" 'helpful-at-point )

(setq fn-last-helpful-buffer nil)

(defun fn-find-helpful-buffer ()
  (interactive)
  (if (buffer-live-p fn-last-helpful-buffer)
	  fn-last-helpful-buffer
	(loop for buffer being the buffers
		  do (let* ((bname (buffer-name buffer)))
			   (when (and (= ?* (aref bname 0))
						  (string-match "^\\*helpful" bname)
						  )
				 (setq fn-last-helpful-buffer buffer)
				 (return buffer)
				 )))))

(defun fn-toggle-helpful-window ()
  (interactive)
  (let ((helpful-buffer (fn-find-helpful-buffer)))
	(when (not (null helpful-buffer))
	  (let ((helpful-window (get-buffer-window helpful-buffer)))
		(if (null helpful-window)
			(pop-to-buffer helpful-buffer)
		  (delete-window helpful-window)
		  )))))

(spacemacs/set-leader-keys "-" 'fn-toggle-helpful-window)

;; around-advice helpful-visit-reference
;;   overwrite find-file advice
;;   remove advice afterwards

(use-package evil-collection-helpful-mod
  :load-path "packages")
(require 'evil-collection-helpful-mod)
(evil-collection-helpful-mod-setup)

;; q to close helpful buffers
(require 'helpful)
(setq helpful-mode-map (delq (assoc 113 helpful-mode-map) helpful-mode-map))
(evil-define-key 'normal helpful-mode-map "q" 'delete-window)


;; make helpful buffer dedicated as soon as it is opened,
;; TODO:  better way to do this than this? use purpose-mechanisms, if available?
(defun helpful-dedicate (f &rest args)
  (apply f args)
  (purpose-set-window-purpose-dedicated-p (selected-window) t)
  )
(advice-add 'helpful-at-point :around 'helpful-dedicate)
(advice-add 'helpful-variable :around 'helpful-dedicate)
(advice-add 'helpful-key :around 'helpful-dedicate)
(advice-add 'helpful-function :around 'helpful-dedicate)

;; key bindings for helpful
;; (spacemacs/set-leader-keys "hdf" 'counsel-describe-function)
;; (spacemacs/set-leader-keys "hdv" 'counsel-describe-variable)
;; ...set by spacemacs, somewhere sometime... :/
(run-with-timer 5 nil #'(lambda ()
						  (spacemacs/set-leader-keys "hdf" 'helpful-function)
						  (spacemacs/set-leader-keys "hdv" 'helpful-variable)))

;;;; other stuff

(defun fn-repl-erase-to-beginning-of-line ()
  (interactive)
  (when (not (equal (line-number-at-pos) (evil-ex-last-line)))
	(end-of-buffer))
  (let ((range (save-excursion  (- (point) (progn (beginning-of-line) (point)))))
		(inhibit-read-only t))
	(backward-delete-char range)))

;; TODO: fix
;; (evil-define-key 'insert 'inferior-python-mode-map (kbd "C-u") 'fn-repl-erase-to-beginning-of-line)

;; == message buffer toggle ======

;; SPC-[ to toggle message buffer in current window (for quick glances)
;; TODO:  want to have this behaviour for all "those" popup keys?

;; -> access, dahin wo auch das repl ding...

(setq fn-toggle-messages-buffer-last nil)
(defun fn-toggle-messages-buffer ()
  "Toggle display of messages buffer in current window."
  (interactive)
  (if (string= (buffer-name) "*Messages*")
	  (progn
		(when (eq (selected-window) (car fn-toggle-messages-buffer-last))
		  (switch-to-buffer (cdr fn-toggle-messages-buffer-last))
		  ))
	(progn
	  (setq fn-toggle-messages-buffer-last (cons (selected-window) (current-buffer)))
	  (switch-to-buffer "*Messages*")
	  )))

(spacemacs/set-leader-keys "wm" 'fn-toggle-messages-buffer)
(global-set-key (kbd "<f10>") 'helpful-key)

;; compatibility (emacs27, helpful)
(defalias 'format-proper-list-p 'proper-list-p)
(require 'ivy				  ;; ineffective
         (setq ivy-height 6)) ;; ineffective :/

(defun fn-configure-ivy ()
  (interactive)
  (setq ivy-height 6)

  (set-face-background 'ivy-current-match "#ac443f")
  (set-face-foreground 'ivy-current-match "#1d1d1a")
  (set-face-inverse-video 'ivy-current-match nil)
  (set-face-foreground 'ivy-modified-buffer "#cf7c43")
  (set-face-foreground 'ivy-minibuffer-match-highlight "#fbcb41")

  (set-face-foreground 'minibuffer-prompt "#337ebe")
  (setq ivy-initial-inputs-alist nil)
  )
(run-with-idle-timer 2 nil 'fn-configure-ivy)
;; -> general utils (oder so)

;; open file, position cursor, flash line
(defun find-file-flash-line (fpath line column)
  (interactive)
  (find-file fpath)
  (goto-line line)
  (evil-goto-column column)
  (pulse-momentary-highlight-one-line (point))
  )
(global-set-key (kbd "C-k") 'spacemacs/evil-smart-doc-lookup)
(with-eval-after-load "counsel.el"
  (spacemacs/set-leader-keys "hdd" 'counsel-apropos))






