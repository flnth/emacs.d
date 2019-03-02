;; modules/lang/markup/config.el    -*- lexical-binding: t; -*-


;;;; markdown-mode

;; -> markup module
;; markdown file

(add-hook 'markdown-mode-hook
		  #'(lambda ()
			  ;; markdown-mode fix
			  (evil-define-key '(normal insert evilified) markdown-mode-map (kbd "M-h") (lambda () (interactive) (fn-navigate 'left)))
			  (evil-define-key '(normal insert evilified) markdown-mode-map (kbd "M-j") (lambda () (interactive) (fn-navigate 'below)))
			  (evil-define-key '(normal insert evilified) markdown-mode-map (kbd "M-k") (lambda () (interactive) (fn-navigate 'above)))
			  (evil-define-key '(normal insert evilified) markdown-mode-map (kbd "M-l") (lambda () (interactive) (fn-navigate 'right)))

			  ;; remark-mode fix
			  (evil-define-key '(normal insert evilified) remark-mode-map (kbd "M-h") (lambda () (interactive) (fn-navigate 'left)))
			  (evil-define-key '(normal insert evilified) remark-mode-map (kbd "M-j") (lambda () (interactive) (fn-navigate 'below)))
			  (evil-define-key '(normal insert evilified) remark-mode-map (kbd "M-k") (lambda () (interactive) (fn-navigate 'above)))
			  (evil-define-key '(normal insert evilified) remark-mode-map (kbd "M-l") (lambda () (interactive) (fn-navigate 'right)))
			  ))

(evil-define-key '(normal insert evilified) markdown-mode-map
  (kbd "M-RET") 'markdown-insert-header-dwim
  (kbd "M-,") 'markdown-promote
  (kbd "M-.") 'markdown-demote
  (kbd "M-n") 'markdown-outline-next
  (kbd "M-p") 'markdown-outline-previous
  (kbd "M-<tab>") 'evil-toggle-fold
  (kbd "C-c C-c") 'markdown-do
  )

;;;; restructured text
(delete '("\\.te?xt\\'" . text-mode) auto-mode-alist)
(add-to-list 'auto-mode-alist '("\\.txt\\'" . rst-mode) t)
(add-to-list 'auto-mode-alist '("\\.te?xt\\'" . text-mode) t)

;;;; yaml-mode

;; -> markup module
;; yaml file

;; ansible-mode
(defun fn--enable-ansible-mode-if ()
  "Enables ansible mode path contains strings."
  (let* ((path (buffer-file-name))
		 (strings '("deployment" "tasks" "play" "book" "ansible" "vars" "deploy" "provision"))
		 (matched (loop for s in strings
						if (string-match-p s path)
						do (return t))))
	(when matched
	  (ansible 1)
	  (font-lock-mode -1)
	  (font-lock-mode 1))))

(add-hook 'yaml-mode-hook #'fn--enable-ansible-mode-if )

(run-with-timer 5 nil #'(lambda ()
						  (use-package origami
							:load-path "packages/origami")
						  ;; folding
						  (add-hook 'yaml-mode-hook #'origami-mode)))

;; keys
(evil-define-key '(normal visual insert) yaml-mode-map
  (kbd "C-<tab>") 'origami-toggle-node
  (kbd "M-n") 'origami-forward-fold-same-level
  (kbd "M-p") 'origami-backward-fold-same-level)


