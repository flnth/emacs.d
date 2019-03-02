;; modules/core/paths/config.el    -*- lexical-binding: t; -*-

;; -> load paths. Maybe do prevent having to walk directories, hardcode them.
;; (I remember something about performance in that context)

;; -> directory magement, load path stuff
(setq dir_system (getenv "DIR_SYSTEM"))
(setq dir_emacs (getenv "DIR_EMACSD"))

(add-to-list 'load-path dir_emacs)

(add-to-list 'load-path (concat dir_emacs "/packages/f.el"))

(add-to-list 'load-path (concat dir_system "/emacs/pjson/"))
(add-to-list 'load-path (concat dir_system "/emacs/whist.el/"))
(add-to-list 'load-path (concat dir_system "/emacs/evil-collection/"))
(add-to-list 'load-path (concat dir_system "/emacs/lsp-python-ms/"))

(load-file (concat dir_system "/emacs/gtypist-mode.el"))

