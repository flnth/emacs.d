;; modules/feature/redmine/config.el    -*- lexical-binding: t; -*-




;; bug-reference mode for indurad redmine
(setq bug-reference-bug-regexp "\\(#\\)\\([0-9]+\\(?:#[0-9]+\\)?\\)")

(if (equal (getenv "LOC") "work")
	(setq bug-reference-url-format "https://redmine.indurad.x/issues/%s")
  (setq bug-reference-url-format "http://demo.redmine.org/issues/%s"))
