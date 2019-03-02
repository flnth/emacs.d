;; modules/editor/general/config.el    -*- lexical-binding: t; -*-

(defun fill-to-column (char column)
  (interactive)
  (save-excursion
    (end-of-line)
    (while (< (current-column) column)
      (insert-char char))))
;; drag-stuff  (M-up, M-down, ...)
(use-package drag-stuff
  :load-path "packages/drag-stuff.el")
;;(drag-stuff-define-keys)
(define-key evil-motion-state-map (kbd "<M-up>") 'drag-stuff-up)
(define-key evil-motion-state-map (kbd "<M-down>") 'drag-stuff-down)

