;; -*- lexical-binding: t -*-

;; (use-package mu4e-alert
;;   :ensure t
;;   ;; :config
;;   ;; (setq mu4e-alert-interesting-mail-query "flag:unread AND NOT flag:trashed")
;;   ;; (setq mu4e-alert-modeline-formatter 'mail-sidebar--segment-formatter)
;;   ;; (mu4e-alert-enable-mode-line-display)
;;   )

;; (defun mail-sidebar--segment-formatter (count)
;;   (when (not (zerop count))
;;     (concat
;;      (propertize (all-the-icons-faicon "envelope" :height 0.9 :v-adjust 0.1)
;; 				   'face `(:family ,(all-the-icons-faicon-family) :inherit mode-line)
;; 				   'help-echo (concat (if (= count 1) "You have an unread email" (format "You have %s unread emails" count)) "\nClick here to view " (if (= count 1) "it" "them"))
;; 				   'mouse-face 'mode-line-highlight
;; 				   'keymap '(mode-line keymap
;; 									   (mouse-1 . mu4e-alert-view-unread-mails)
;; 									   (mouse-2 . mu4e-alert-view-unread-mails)
;; 									   (mouse-3 . mu4e-alert-view-unread-mails))) " "
;;      (propertize (format "%d" count) 'face `(:inherit mode-line)))))
