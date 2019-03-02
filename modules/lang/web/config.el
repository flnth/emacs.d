;; modules/lang/web/config.el    -*- lexical-binding: t; -*-





;;;; web-mode

;; -> module for everything web? (html-mode,web-mode,etc?)

(require 'web-mode)

(set-face-foreground 'web-mode-html-tag-bracket-face "#cf7c43")

(set-face-foreground 'web-mode-html-attr-name-face "#cc6666")
(set-face-attribute 'web-mode-html-attr-name-face nil :slant 'italic)

(set-face-foreground 'web-mode-html-tag-face "#5983ab")

(set-face-foreground 'web-mode-css-selector-face "#98bd5e")
