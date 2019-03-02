;; -*- lexical-binding: t -*-

;;; bookmarks
;; mu4e bookmarks in the sidebar
;; (defun fn-format-mu4e-bookmarks ()
;;   (mapconcat
;;    (lambda (bm)
;; 	 (mu4e~main-action-str
;; 	  (concat "\t* [b" (make-string 1 (mu4e-bookmark-key bm)) "] "
;; 			  (mu4e-bookmark-name bm))
;; 	  (concat "b" (make-string 1 (mu4e-bookmark-key bm)))))
;;    (mu4e-bookmarks) "\n"))

;; TODO:
;;   - change format in mail-sidebar
;;   - propertize the strings so I can open up views by pressing enter
;;     code for opening:
;;         (mu4e-headers-search-bookmark (mu4e-get-bookmark-query ?u))

;;; custom highlighting and fontification

(defface mu4e-maildirs-extension-maildir-root-face
  '((t (:foreground "red")))
  "Face used to display the root of ...")

(defun fn-append-right-aligned (s1 s2 available-width)
  "Appends string s2 to s1 but in such a way as to have s2
right-aligned to the maximum available width for the result."
  (format (format "%%s%%%ds" (- available-width (length s1) 1)) s1 s2))

(defun fn-mu4e-maildirs-extension-propertize-handler (m)
  "Propertize the maildir text using M plist.
MODDED: assume that mu4e-maildirs-extension-maildir-format ends
with `|%u'."
  (let* ((fmt mu4e-maildirs-extension-maildir-format)
         (hl-regex mu4e-maildirs-extension-maildir-hl-regex)
         (hl-p (funcall mu4e-maildirs-extension-maildir-hl-pred m)))
	(setq fmt (propertize fmt 'face 'mu4e-maildirs-extension-maildir-face))

	;; highlight name
	(when (eq (plist-get m :level) 0)
	  (setq fmt (replace-regexp-in-string "%n"
										  (propertize "%n"
													  'face
													  'mu4e-maildirs-extension-maildir-root-face)
										  fmt)))

	;; highlight unread
    (when hl-p
      (setq fmt (replace-regexp-in-string hl-regex
                                          (propertize hl-regex
                                                      'face
                                                      'mu4e-maildirs-extension-maildir-hl-face)
										  fmt)))

	(let* ((full-string (format-spec fmt (funcall mu4e-maildirs-extension-maildir-format-spec m)))
		   (sliced (s-slice-at "|" full-string))
		   (left (car sliced))
		   (right (s-chop-prefix "|" (cadr sliced)))
		   (unread (ignore-errors (string-to-number right)))
		   (right (if (and unread (> unread 0)) right "")))
	  (fn-append-right-aligned left right mail-sidebar-window-width))))

(setq mu4e-maildirs-extension-propertize-func #'fn-mu4e-maildirs-extension-propertize-handler)
