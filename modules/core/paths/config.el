;; modules/core/paths/config.el    -*- lexical-binding: t; -*-

;; ---------------------------------------------------------
;; "The root of this system's directory stack. Defaults to the
;; value of the environment variable DIR_STACKROOT, or if that is not
;; set, the user's home directory."
(setq dir_stackroot
	  (let ((d (f-slash (getenv "STACKROOT"))))
		(if (null d)
			(f-slash (getenv "HOME"))
		  (when (not (f-exists? d))
			(make-directory d t))
		  d)))

;; "The root of this system's emacs directory stack. Defaults to the
;; value of the environment variable DIR_EMACSD, of if that is not
;; set, the user's home directory."
(setq dir_emacs
	  (let ((d (f-slash (getenv "DIR_EMACSD"))))
		(if (null d)
			(concat (f-slash (getenv "HOME")) ".emacs.d/")
		  (when (not (f-exists? d))
			(make-directory d t))
		  d)))

;; ---------------------------------------------------------
(add-to-list 'load-path dir_emacs)
(add-to-list 'load-path (concat dir_emacs "/packages/f.el"))

(add-to-list 'yas-snippet-dirs
			 (concat dir_emacs "/share/yasnippets" ))


