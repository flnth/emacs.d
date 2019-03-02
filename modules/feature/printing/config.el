;; modules/feature/printing/config.el    -*- lexical-binding: t; -*-




;;;; printing (feature/printing)
;; (require 'ps-print)

;; (setq ps-paper-type 'a4)

;; print format
;; (setq ps-landscape-mode 'landscape)  ;; nil: portrait
;; (setq ps-number-of-columns )

;; header/footer
;; (setq ps-print-header nil)
;; (setq ps-print-header t)
;; ps-header-frame-alist -->  fore-color,back-color,shadow-color,border-color,border-width
;; use customization to change
;;  ps-header-font-family, ps-header-font-size, ps-header-title-font-size : change
;; ps-header-lines:  change amount of information in heaeder (1: buffer name/page number only)
;; ps-left-header, ps-right-header :  change content

;; double-sided printing
;; ps-spool-duplex:  double sided printing

;; line numbers
;; ps-line-number, ps-line-number-color, ps-line-number-font, ps-line-number-font-size

;; zebra printing
;; ps-zebra-stripe-height , ps-zebra-stripes t -> print zebra stripes, ps-zebra-color, ps-zebra-stripe-follow

;; fonts
;; add new font using ps-font-info-database
;; ps-font-family  (check messages buffer if invalid)
;; ps-font-size    (individual for landscape and portrait size, landscape . portrait)
;; ps-header-font-family, ps-header-font-size,...
;; ps-line-spacing, ps-paragraph-spacing

;; faces
;; force some faces to be printed a certain way:  ps-bold-faces, ps-italic-faces, ps-underline-faces
;; ps-build-face-reference to rebuild/update internal structures
;; ps-use-face-background  :  t nil
;; ps-default-fg, ps-default-bg
;; ps-print-color-p -> nil for black/white printing, also see ps-black-white-faces
;;
;; (additional face attributes for printing:  box,outline,overline,shadow)
;; example:  remap keyword face to another foreground color and bold attribute:
;;    (ps-extend-face '(font-lock-keyword-face "RoyalBlue" nil bold) 'MERGE)
;; to use a new face, define with defface, then call ps-extend-face with it.

;; see ps-setup, ps-print-customize for convenience



;; (when (executable-find "ps2pdf")
;;   (defun modi/pdf-print-buffer-with-faces (&optional filename)
;;     "Print file in the current buffer as pdf, including font, color, and
;; underline information.  This command works only if you are using a window system,
;; so it has a way to determine color values.

;; C-u COMMAND prompts user where to save the Postscript file (which is then
;; converted to PDF at the same location."
;;     (interactive (list (if current-prefix-arg
;;                            (ps-print-preprint 4)
;;                          (concat (file-name-sans-extension (buffer-file-name))
;;                                  ".ps"))))
;;     (ps-print-with-faces (point-min) (point-max) filename)
;;     (shell-command (concat "ps2pdf " filename))
;;     (delete-file filename)
;;     (message "Deleted %s" filename)
;;     (message "Wrote %s" (concat (file-name-sans-extension filename) ".pdf"))))
