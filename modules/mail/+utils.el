;; -*- lexical-binding: t -*-

(defun fn-wash-mail-citation ()
  "Delete from the first regex match until the end of the buffer"
  (let* ((regex-patterns
          (list
           "On.*Wrote:$"
           "Am.*schrieb.*:$"
           "From.*mailto:.*$"
           "^_+$") ;; TODO: add a multiline regex to match "From:" on the first, and "Sent:" on the next line
          )
         (first-occurences
          (cl-loop for pattern in regex-patterns
                   collect (progn
                             (message-goto-body)
                             (search-forward-regexp pattern nil t)))))
    ;; alternatively:
    ;; (first-occurences
    ;;  (mapcar (lambda (pattern)
    ;;            (progn
    ;;              (beginning-of-buffer)
    ;;              (search-forward-regexp pattern nil t)))
    ;;          regex-patterns)))
    (setq first-occurences (sort (-non-nil first-occurences) '<))
    (when (not (null first-occurences))
      (goto-char (car first-occurences))
      (beginning-of-line)
      (kill-region (point) (point-max))
      )
    (end-of-buffer)
    (activate-mark)
    (message-goto-body)
    )
  )

(defun fn--mail-cite-function ()
  "Called by mu4e to cite."
  (message-cite-original-without-signature)
  (goto-char (point-min))
  (replace-regexp "^" "> ")
  )

(defun fn-message-kill-buffer ()
  "Kill the message buffer and close window (dwim)."
  (interactive)
  (message-kill-buffer)
  (when (> (count-visible-buffers) 1) ;; TODO:  only close window if it was one of those temporaries
    (delete-window)
    )
  )

(defun fn-message-notmuch-mua-send-and-exit ()
  "Send message and close window (dwim)."
  (interactive)
  (notmuch-mua-send-and-exit)
  (when (> (count-visible-buffers) 1)   ;; TODO:  only close window if it was one of those temporaries
    (delete-window)
    )
  )

;; (define-key notmuch-message-mode-map (kbd "C-c C-k") 'fn-message-kill-buffer)
;; (define-key notmuch-message-mode-map (kbd "C-c C-c") 'fn-message-notmuch-mua-send-and-exit)

;; remove attribution in citations for supercite
(defun fn-sc-attribution-post-selection-hook ()
  (setq attribution "")
  (setq citation ">"))

(defun fn-line-quotation-level ()
  "Count the number of '>' at the beginning of the current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (let* ((line-beginning
            (progn
              (beginning-of-line)
              (point))
            ))
      (end-of-line)
      (search-backward-regexp "^ *>*" line-beginning t nil )
      (- (match-end 0) (match-beginning 0))
      )
    )
  )

(defun fn-message-fill ()
  "Fills the paragraphs in the current message buffer."
  ;; if filling needs more customization, check
  ;; https://stackoverflow.com/questions/11012634/auctex-custom-fill-nobreak-predicate-hook-to-respect-braces
  ;; https://emacs.stackexchange.com/questions/12392/prevent-fill-paragraph-from-breaking-latex-citations-in-org-mode/12419
  (let ((fill-column 5000))
    (save-restriction
      (goto-char 1)
      (search-forward-regexp (concat "^" (regexp-quote outlook-style-conf-start) "$") nil t)
      (narrow-to-region 1 (point))
      (message-fill-yanked-message)
      )
    )
  )

(defun fn-wrap-citations ()
  "Wraps citations in the current message buffer in <quote> html tags."
  (interactive)
  (goto-char 1)
  (while (search-forward-regexp "^ *> *" nil t)
    (let ((q0 nil)
          (q1 nil)
          (qlines (list)))
      (while (eq q0 q1)  ;; TODO:  nesting... -> while q0 < q1, recursively call, for that refactor this
        (setq qlines (append qlines (list (line-number-at-pos))))
        (setq q0 (fn-line-quotation-level))
        (next-line)
        (setq q1 (fn-line-quotation-level))
        )
      (setq qlines (remove-duplicates qlines))
      (fn-quote-qlines qlines)
      )
    )
  )

;; while( goto first citation line)
;;        Q0 := get current quotation level
;;        (while Q1 := next line quotation level == Q0)
;;               goto next line
;;               qlines += current

;;         quote all qlines


(defun fn-quote-qlines (qlines)
  "Wrap the qlines referred by the line numbers in qlines in <quote> tags."
  (goto-line (+ 1 (car (last qlines))))
  (open-line 1)
  (insert "</quote>")
  (cl-loop for line in (reverse qlines)
           do
           (goto-line line)
           (fn-remove-citation-prefixes))
  ;; (goto-line (- (car qlines) 1))
  (goto-line (car qlines))
  (open-line 1)
  (insert "<quote>")
  )

(defun fn-remove-citation-prefixes ()
  "Remove all citation prefixes from the current line."
  (interactive)
  (end-of-line)
  (search-backward-regexp "^ *> *>* *>* *>* *")
  (delete-char (- (match-end 0) (match-beginning 0)))
  )



(defun fn-insert-signature (signature-file)
  (goto-char 1)
  (if (search-forward-regexp "%sig" nil t)
      (progn
        (kill-whole-line)
        (insert-file-contents signature-file)
        (open-line 1)
        ))
  )


;; TODO:  can I swap redmine@mail.indurad.com for redmine in headers view?
;; (defun contact-rewrite-function (contact)
;;   (when (string= "redmine@mail.indurad.com")
;; 	(message (plist-get contact :name))
;; 	))

;; (setq mu4e-contact-rewrite-function #'contact-rewrite-function)
;; (setq mu4e-contact-rewrite-function nil)


(defun fn-remove-indurad-plaintext-signature ()
  (interactive)
  (goto-char 1)
  (while (search-forward-regexp "^[> ]*_______________________________________$" nil t)
    ;; note line, find next one
    (let ((first-line (line-number-at-pos)))
      (when (search-forward-regexp "^[> ]*_______________________________________$" nil t)
        (let* ((second-line (line-number-at-pos))
               (diff (- second-line first-line))))
          (when (< diff 18)
            (goto-line first-line)
            (kill-line diff)
            )
        )
      )
    )
  )

(defun htmlize-string (sourceCodeStr langModeName)
  "Take SOURCECODESTR and return a htmlized version using LANGMODENAME.
This function requries the htmlize.el by Hrvoje Niksic."
  (require 'htmlize)
  (let (htmlizeOutputBuf p1 p2 resultStr)

    ;; put code in a temp buffer, set the mode, fontify
    (with-temp-buffer
      (insert sourceCodeStr)
      (funcall (intern langModeName))
      (font-lock-fontify-buffer)
      (setq htmlizeOutputBuf (htmlize-buffer))
      )

    ;; extract the fontified source code in htmlize output
    (with-current-buffer htmlizeOutputBuf
      (setq p1 (search-forward "<pre>"))
      (setq p2 (search-forward "</pre>"))
      (setq resultStr (buffer-substring-no-properties (+ p1 1) (- p2 6))))

    (kill-buffer htmlizeOutputBuf)
    resultStr
    ))


;;;; automatische anrede

;; (defun
;;     get-recipient-for-body ()
;;   (let
;;       * ((to (message-fetch-field
;;               "To "))
;;          (comps (mail-extract-address-components to)))
;;       (if to
;;           (shorten-realname (
;;                              nth
;;                              0 comps))
;;         "")))
;; (defun
;;     my-fill-body ()
;;   (let
;;       ((recipient (get-recipient-for-body)))
;;     (save-excursion
;;       (goto-char (point-max))
;;       (insert (
;;                format
;;                "Hi  %s,\ n\n" recipient)))))
;; (add-hook 'message-signature-setup-hook 'my-fill-body)


;;;; Automatische wahl des senders?

;; returns nil if PATTERN is not contained in any member of HEADERS
;; (defun any-value-matches (pattern headers)
;;   (if (null headers)
;;       nil
;;     (or
;;      (string-match pattern (car headers))
;;      (any-value-matches pattern (cdr headers)))))

;; (defun determine-sender-and-reply (r-all)
;;   (let* ((headers (notmuch-show-get-prop :headers nil))
;;          (vals (plist-values headers))
;;          (sender (cond
;;                   ((any-value-matches
;;                     "debian\\.  org  " vals)
;;                    "Michael Stapelberg <stapelberg@debian.org  >")
;;                   ((any-value-matches "i3 " vals)
;;                    "Michael Stapelberg <michael@i3wm.org  >")
;;                   (t nil))))
;;       (notmuch-mua-reply (notmuch-show-get-message-id) sender r-all)))

;; (define-key notmuch-show-mode-map"r"
;;   (lambda ()
;;     (interactive)
;;     (determine-sender-and-reply nil)))

;;; message-mode (archived)

;; combined citation and outlook-style setup hook
(defun fn-message-setup ()

  (fn-wash-mail-citation)
  ;; (message-cite-original-without-signature)
  (sc-cite-original)
  ;; (gnus-article-fill-cited-article 80)

  (message-goto-body)
  (newline 3)

  ;; (outlook-style--gnus-prepare)
  (outlook-style--gnus-prepare)
  ;; (fn-select-mail-citation)
  ;; select first text occurence after headers, until last line before the gnus-prepare stuff

  ;; reasonable visual line navigation
  (setq fill-column 100)
  ;; (spacemacs/toggle-visual-line-navigation-on)
  (visual-line-mode)
  (visual-fill-column-mode)
  )

;; overwrite for performance, ignore C-S-h / C-S-l
;; (run-with-timer 2 nil #'(lambda ()
;; 						  (defun mu4e~headers-get-loading-buf ()
;; 							"Get a buffer to give feedback while loading a message view."
;; 							(unless (buffer-live-p mu4e~headers-loading-buf)
;; 							  (setq mu4e~headers-loading-buf
;; 									(get-buffer-create " *mu4e-loading*")))
;; 							(with-current-buffer mu4e~headers-loading-buf
;; 							  ;; (evil-local-set-key 'normal (kbd "C-S-h") 'ignore)
;; 							  ;; (evil-local-set-key 'normal (kbd "C-S-l") 'ignore)
;; 							  (setq header-line-format nil)
;; 							  (read-only-mode)
;; 							  (let ((inhibit-read-only t))
;; 								(erase-buffer)
;; 								(local-set-key (kbd "q")
;; 											   (if (eq mu4e-split-view 'single-window)
;; 												   'kill-buffer
;; 												 'kill-buffer-and-window))
;; 								(insert (propertize "Waiting for message..."
;; 													'face 'mu4e-system-face 'intangible t))))
;; 							mu4e~headers-loading-buf)))
