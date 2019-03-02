;; modules/feature/eshell/config.el    -*- lexical-binding: t; -*-

(use-package eshell
  ;; :demand
  ;; :bind (:map eshell-mode-map
  :commands (eshell
			 eshell-mode)
  ;; 			  ("C-l" . +eshell-clear))
  :init
  ;; (spacemacs/set-leader-keys "'" 'eshell)
  (format ".")

  :config
  (require 'em-alias)
  (require 'em-term)

  (setq eshell-where-to-jump 'begin
		eshell-review-quick-commands nil
		eshell-smart-space-goes-to-end t)

  ;; disable git for now, makes it way slow...
  (defun epe-git-p () nil)
  ;; TODO: also consider disabling spacemacs shell layer...

;;; prompt
  (setq eshell-prompt-function
		(lambda nil
		  (concat
		   (if (eq eshell-last-command-status 0)
			   (propertize  "▲ " 'face `(:foreground "#aaaaaa"))
			 (propertize  "◮ " 'face `(:foreground "#454dd0")))
		   (propertize (eshell/pwd) 'face `(:foreground "#cf7c43"))
		   " "))
		eshell-prompt-regexp  "^[▲◮] [^\s]* "
		eshell-pwd-convert-function
		(lambda (s)
		  (replace-regexp-in-string (concat "^" (getenv "HOME")) "~" s))
		eshell-banner-message ""
		)

;;; hook
  (defun +eshell-mode-hook ()
	(setq header-line-format nil)
	(vi-tilde-fringe-mode -1)
	(toggle-truncate-lines 1)
	;; enable colors
	(setenv "TERM" "emacs logic configuration"))

  (add-hook 'eshell-mode-hook #'+eshell-mode-hook)

;;; completion
  ;; https://github.com/JonWaltman/pcmpl-args.el
  ;; https://melpa.org/#/?q=pcmp
  ;; https://tsdh.wordpress.com/2013/05/31/eshell-completion-for-git-bzr-and-hg/
  (when (and (executable-find "fish"))
	(use-package fish-completion
	  :load-path "packages/emacs-fish-completion")
	(global-fish-completion-mode))

;;; history
  ;; (eshell-previous-matching-input)
  ;; (eshell-previous-matching-input-from-input)

  ;; not in eshell?:
  ;; comint-history-isearch-backward-regexp

;;; command interpretation
  (mapcar (lambda (elem)
  			(add-to-list 'eshell-visual-commands elem))
  		  '("pico" "nano"))
  ;; change eshell current directory from the outside:
  ;; (with-current-buffer eshellbuf (cd "/some/path") (eshell-emit-prompt)))
;;; outside control

  (defun eshell-cwd (eshell-bufname)
	"Sets the eshell directory to the current buffer
  Usage: M-x eshell-cwd"
	(interactive)
	(let ((path (file-name-directory (or  (buffer-file-name) default-directory))) )
      (with-current-buffer eshell-bufname
		(cd path)
		(eshell-emit-prompt))))

;;; alias / commands
  ;; put in ~/.Eshell/alias?

  ;; find-file -> ee
  ;; find-file-other-window -> e   TODO: custom fun to open in 1) assoc-win, and/or 2) last active

  (defun eshell/test ()
	(message "hello!"))

  ;; (defun eshell/red (arg)
  ;; 	(message (format "%s" arg)))

  (defun eshell/x ()
	(insert "exit")
	(eshell-send-input)
	(delete-window))

  (defun eshell/z (&rest args)
	(setq A args)
	(cd (car
		 (split-string
		  (shell-command-to-string
		   (concat "fasd -lR -d "
				   (string-join args " ")))
		  "\n" t)))
	nil)

  (defun eshell/__up ()
	(interactive)
	;; (funcall-interactively 'evil-ret)
	(let ((inhibit-read-only t))
	  (move-to-column 0)
	  (kill-line)
	  (cd "..")
	  (eshell-emit-prompt)))

  (defun eshell/__down ()
  	(interactive)
  	;; (funcall-interactively 'evil-ret)
  	;; (cd (car (ring-elements eshell-last-dir-ring)))
  	;; ;; (evil-delete-line)
  	;; (eshell-emit-prompt)
  	nil)

  (defun eshell/e (filename)
	"Open file in best-matching buffer."
	;; TODO: move to where it fits better, requires stuff from access, access
	;; requires eshell, requires utils

	;; TODO:  maybe just change to (window-eshell -> associated), (frame-eshell -> last-mru)

	(let* ((buf (find-file-noselect filename))
		   (last-mru-win (get-last-mru-window-among (window-list)))
		   (win (or (and
					 last-mru-win
					 (not (window-dedicated-p last-mru-win))
					 (not (+utils-side-window-p last-mru-win))
					 last-mru-win)
					(+access-eshell-assoc-win-from-eshell-win (selected-window)))))
	  (without-purpose
		(select-window win)
		(switch-to-buffer
		 (if (listp buf) (car buf) buf)))))

  ;; -- fasd:  track directories
  (setq +eshell--fasd-cd-last-dir "")

  (defun +eshell--fasd-cd-hook (&rest args)
	(when (not (string= default-directory +eshell--fasd-cd-last-dir))
	  (start-process "*fasd*" nil "fasd" "--add" default-directory)
	  (setf +eshell--fasd-cd-last-dir default-directory)))

  (advice-add #'eshell/cd :after #'+eshell--fasd-cd-hook)
;;; keys / commands

  (defun +eshell-clear ()
	(interactive)
	(let ((eshell-buffer-maximum-lines 0))
	  (eshell-truncate-buffer))
	(goto-char (point-max))
	)

  (defun +eshell-line-discard ()
	(interactive)
	(eshell-bol)
	(let ((done nil)
		  (last-pos (point)))
	  (while (not done)
		(condition-case ex
			(progn
			  (kill-line)
			  (setf done t))
		  ('text-read-only
		   (goto-char last-pos)
		   (search-forward-regexp "[\s]+[^\s]")
		   (goto-char (- (point) 1))
		   (setf last-pos (point)))))))

  ;; key bindings
  (defun +eshell-keybindings ()
	(evil-define-key '(normal insert motion visual) eshell-mode-map (kbd "C-l") #'+eshell-clear)
	(evil-define-key '(insert) eshell-mode-map (kbd "C-u") #'+eshell-line-discard)
	(evil-define-key '(normal insert motion visual) eshell-mode-map (kbd "C-p") #'eshell-previous-matching-input-from-input)
	(evil-define-key '(normal insert motion visual) eshell-mode-map (kbd "C-n") #'eshell-next-matching-input-from-input)
	(evil-define-key '(normal) eshell-mode-map (kbd "dd") #'+eshell-line-discard)
	(evil-define-key '(normal) eshell-mode-map (kbd "G") #'(lambda () (interactive) (goto-char (point-max))))

	(evil-define-key '(normal insert motion visual) eshell-mode-map (kbd "C-<up>") #'eshell/__up )
	(evil-define-key '(normal insert motion visual) eshell-mode-map (kbd "C-<down>") #'eshell/__down)
	(evil-define-key '(normal insert motion visual) eshell-mode-map (kbd "C-<left>") #'eshell/__down)

	(define-key eshell-mode-map (kbd "<tab>") (lambda () (interactive) (pcomplete-std-complete)))
	(define-key eshell-mode-map  (kbd "C-u") #'+eshell-line-discard)
	(define-key eshell-mode-map  (kbd "C-l") #'+eshell-clear)
	(define-key eshell-mode-map  (kbd "C-n") #'eshell-next-matching-input-from-input)
	(define-key eshell-mode-map  (kbd "C-p") #'eshell-previous-matching-input-from-input)


	(define-key eshell-mode-map (kbd "C-<up>") #'eshell/__up )
	(define-key eshell-mode-map (kbd "C-<down>") #'eshell/__down )
	(define-key eshell-mode-map (kbd "C-<left>") #'eshell/__down )

	)

  (add-hook 'eshell-first-time-mode-hook #'+eshell-keybindings)
  ;; (add-hook 'eshell-mode-hook #'+eshell-keybindings)

  ;; TODO: navigation, open a shell in some direction for GUI emacs
  (defun +eshell-open-in-direction ()
	nil)

  ;; TODO: C-u to not delete the prompt...
  ;; TODO: z command either manually or using https://gitlab.com/emacs-stuff/fasd-shell

  ;; -------------------------------------------------------
;;;; ls-coloring

  (defface +eshell-source-file-face
	'((t (:foreground "Green")))
	"Eshell face for code (.c, .f90 etc) files.")

  (defface +eshell-img-face
	'((t (:foreground "magenta" :weight bold)))
	"Eshell face for image (.jpg etc) files.")

  (defface +eshell-movie-face
	'((t (:foreground "white" :weight bold)))
	"Eshell face for movie (.mpg etc) files.")

  (defface +eshell-music-face
	'((t (:foreground "magenta")))
	"Eshell face for music (.mp3 etc) files.")

  (defface +eshell-ps-face
	'((t (:foreground "cyan")))
	"Eshell face for PostScript (.ps, .pdf etc) files.")



  ;; (setq +eshell-archives-compressed
  ;; 		'("tar" "tgz" "zip" "bz2" "bz" "tz" "7z" "gem" "apk" "rpm" "deb" "jar" "rpm" "xz" "gz" "z")
  ;; 		+eshell-img-list
  ;; 		'("jpg" "jpeg" "png" "gif" "bmp" "ppm" "tga" "xbm" "xpm" "tif" "tiff" "svg" "xcf" "emf" "eps" "ico" "fli")
  ;; 		+eshell-movie-list
  ;; 		'("mov" "mp4" "m4v" "vob" "wmv" "asf" "rm" "flc" "flv" "divx" "mpg" "avi" "gl" "dl")
  ;; 		+eshell-music-list
  ;; 		'("aac" "au" "flac" "mid" "mka" "mp3" "mpc" "ogg" "ra" "wav" "m4a")
  ;; 		+eshell-documents
  ;; 		'("pdf" "html" "doc" "docx" "ppt" "pptx" "odt" "odp" "epub")
  ;; 		+eshell-source-files
  ;; 		'("pl" "PL" "pm" "tt" "yml" "sql" "css" "js" "sh" "R" "r" "rb" "cc" "c" "lua" "vim" "m" "lisp" "py" "hs" "cpp" "el")
  ;; 		+eshell-headers-docs-literates
  ;; 		'("h" "hpp" "1" "tex" "bib")
  ;; 		+eshell-special
  ;; 		'("*Makefile" "*do" "*Doxyfile" "*config" "conf" "cfg")
  ;; 		+eshell-plain-text
  ;; 		'("txt" "md" "org")
  ;; 		+eshell-other-readables
  ;; 		'("xlm" "xls" "xlsx")
  ;; 		+eshell-ps-list
  ;; 		'("ps" "eps" "cps" "pdf")
  ;; 		eshell-ls-highlight-alist nil)

  ;; (let (list face)
  ;; 	(mapcar (lambda (elem)
  ;; 			  (setq list (car elem)
  ;; 					face (cdr elem))
  ;; 			  (add-to-list 'eshell-ls-highlight-alist
  ;; 						   (cons `(lambda (file attr)
  ;; 									(string-match
  ;; 									 (concat "\\." (regexp-opt ,list t) "$")
  ;; 									 file))
  ;; 								 face)))
  ;; 			'((+eshell-source-files . +eshell-source-file-face)
  ;; 			  (+eshell-img-list . +eshell-img-face)
  ;; 			  (+eshell-movie-list . +eshell-movie-face)
  ;; 			  (+eshell-music-list . +eshell-music-face)
  ;; 			  (+eshell-ps-list . +eshell-ps-face))))

  ;; -------------------------------------------------------
  ;;
  ;; TODO: not eshell? something for repls? is this a problem actually?
  ;;
  ;; (defun comint-fix-window-size ()
  ;; "Change process window size."
  ;; (when (derived-mode-p 'comint-mode)
  ;;   (set-process-window-size (get-buffer-process (current-buffer))
  ;;                        (window-height)
  ;;                        (window-width))))

  ;; (defun my-shell-mode-hook ()
  ;;   ;; add this hook as buffer local, so it runs once per window.
  ;;   (add-hook 'window-configuration-change-hook 'comint-fix-window-size nil t))

  ;; (add-hook 'shell-mode-hook 'my-shell-mode-hook)

  )

;; (with-eval-after-load 'eshell
;;   (define-key eshell-mode-map (kbd "C-n") #'eshell-previous-matching-input-from-input)
;;   (define-key eshell-mode-map (kbd "C-p") #'eshell-next-matching-input-from-input))


;;; help

;; see lisp commands
;; eshell-parse-command "echo hello"

;; argument predicates, filtering
;; eshell-display-modifier-help
;; eshell-display-predicate-help

;; extension:
;; eshell-predicate-alist
;; eshell-modifier-alist

;; elisp list handling with modifiers and predicates
;; echo *
;;
;;Attempt to delete minibuffer or sole ordinary window
