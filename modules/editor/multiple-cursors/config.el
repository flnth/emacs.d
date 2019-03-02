;; modules/editor/multiple-cursors/config.el    -*- lexical-binding: t; -*-


;; multiple-cursors -------------------
;; TODO: this might actually be useable, but definitely needs more work
;; https://github.com/magnars/multiple-cursors.el

;; use-cases:
;; 1) create multiple cursors, then do block-wise selection, then e.g. replace all
;; 2) select region, create cursor on every line
;; 3) create cursor for every symbol underneath cursor in a) buffer, b) on screen
;; ...

;; (multiple-cursors-mode)

;;;###autoload
(defun mc/mmlte--up ()
  (interactive)
  (mc/mark-previous-like-this 1)
  (setq mc/mark-more-like-this-extended-direction 'up)
  (mc/mmlte--message))

;;;###autoload
(defun mc/mmlte--down ()
  (interactive)
  (mc/mark-next-like-this 1)
  (setq mc/mark-more-like-this-extended-direction 'down)
  (mc/mmlte--message))

(define-key evil-motion-state-map (kbd "M-S-<up>") 'mc/mmlte--up)
(define-key evil-motion-state-map (kbd "M-S-<down>") 'mc/mmlte--down)

