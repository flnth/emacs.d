;; modules/ui/evil/config.el    -*- lexical-binding: t; -*-




;;;; -- evil-collection ------------------------------------------

;; -> evil module

(add-to-list 'load-path (concat dir_emacs "packages/evil-collection/"))

(require 'evil-collection-minibuffer)
(evil-collection-minibuffer-setup)

(require 'evil-collection-slime)
(evil-collection-slime-setup)
;; -- ??--  .....do configure fucking C-n/C-p in the fucking minibuffer, fuck
;; (evil-define-key 'insert evil-ex-completion-map (kbd "C-p") 'previous-complete-history-element)
;; (evil-define-key 'insert evil-ex-completion-map (kbd "C-n") 'next-complete-history-element)
;; (evil-define-key 'normal evil-ex-completion-map (kbd "C-p") 'previous-history-element)
;; (evil-define-key 'normal evil-ex-completion-map (kbd "C-n") 'next-history-element)

;;   (dolist (map (list minibuffer-local-map
;;                      minibuffer-local-ns-map
;;                      minibuffer-local-completion-map
;;                      minibuffer-local-must-match-map
;;                      minibuffer-local-isearch-map))
;;     (evil-define-key 'normal map (kbd "<escape>") 'abort-recursive-edit)
;;     (evil-define-key 'normal map (kbd "<return>") 'exit-minibuffer)
;;     (evil-define-key 'normal map (kbd "C-n") 'next-history-element)
;;     (evil-define-key 'normal map (kbd "C-p") 'previous-history-element)
;;     )

(require 'evil-collection-ediff)
(evil-collection-ediff-setup)

(require 'evil-collection-calendar)
(evil-collection-calendar-setup)

;; no evil-mode in minibuffer for helm/ivy/counsel:
(defun helm-wrap (f &rest args)
  (remove-hook 'minibuffer-setup-hook 'evil-collection-minibuffer-insert)
  (run-with-idle-timer 0.1 nil (lambda () (interactive)
								 (add-hook 'minibuffer-setup-hook 'evil-collection-minibuffer-insert)))
  (apply f args)
  )
(advice-add 'helm :around 'helm-wrap)

(defun ivy-read-wrap (f &rest args)
  (remove-hook 'minibuffer-setup-hook 'evil-collection-minibuffer-insert)
  (run-with-idle-timer 0.1 nil (lambda () (interactive)
								 (add-hook 'minibuffer-setup-hook 'evil-collection-minibuffer-insert)))
  (apply f args)
  )
(advice-add 'ivy-read :around 'ivy-read-wrap)
;;;;; dired
;; TODO
;; (require 'evil-collection-dired)

;;;;; ibuffer

;; evil collection -> evil

;; (spacemacs/set-leader-keys (kbd "ai") 'ibuffer)

(require 'evil-collection-ibuffer)
(evil-collection-ibuffer-setup)
(require 'evil-collection-compile)
;; -> evil

(global-set-key (kbd "<C-S-pause>") #'evil-jump-forward)
(evil-ex-define-cmd "Q[uit]" 'delete-frame)
;; -- EVIL SEARCH WRAPPER - HIGHLIGHT CURRENT MATCH -------

;; -> evil/search-mod

;; temporary workaround until errors in here not happening anymore (!)
;; TODO: fix!
(defun evil-ex-hl-do-update-highlight (&optional buffer)
  "Timer function for updating the highlights."
  (ignore-errors
	(when (buffer-live-p buffer)
      (with-current-buffer buffer
		(evil-ex-hl-update-highlights)))
	(setq evil-ex-hl-update-timer nil)))

(defvar last-post-command-position 0
  "Holds cursor position from last run of post-command-hooks")
(make-variable-buffer-local 'last-post-command-position)

(defun fn-cleanup-after-highlight-current-match ()
  (when (or (not (equal (point) last-post-command-position))
			(not (equal fn-evil-search-active-window (selected-window) )))
	(internal-show-cursor fn-evil-search-active-window t)
	(remove-hook 'post-command-hook 'fn-cleanup-after-highlight-current-match)
	(fn-reset-evil-ex-search-keys)
	(setq fn-evil-search-active-window nil)))

(defun* fn-find-overlay-with-prio (overlays prio)
  (while overlays
	(let ((overlay (car overlays)))
	  (if (eq (overlay-get overlay 'priority) prio)
		  (return-from fn-find-overlay-with-prio overlay))
	  (setq overlays (cdr overlays)))))

(defun fn-reset-evil-ex-search-keys ()
  "Reset keys to what they were before the search, and show cursor."
  (interactive)
  (global-set-key (kbd "C-g") 'keyboard-quit )
  (define-key evil-normal-state-map (kbd "<escape>") 'evil-force-normal-state))

(defun fn-exit-evil-ex-search ()
  (interactive)
  (internal-show-cursor nil t)
  (spacemacs/evil-search-clear-highlight)
  (fn-reset-evil-ex-search-keys)

  ;; in case buffers were changed in the meantime: do reset
  (keyboard-quit)
  (evil-force-normal-state))

(setq fn-evil-search-active-window nil)

(defun fn-setup-evil-ex-search ()
  (define-key evil-normal-state-map (kbd "<escape>") 'fn-exit-evil-ex-search )
  (global-set-key (kbd "C-g") 'fn-exit-evil-ex-search )
  (unless fn-evil-search-active-window
	(setq fn-evil-search-active-window (selected-window))
	)
  )

(defun* fn-evil-ex-search-apply ()
  "Find correct overlay, set its face to 'evil-ex-search, and
setup environment."
  (let ((overlays (overlays-at (point))))
	(while overlays
	  (let ((overlay (car overlays)))
		(when (eq (overlay-get overlay 'priority) 1000)
		  (progn
			(overlay-put overlay 'face 'evil-ex-search)
			(internal-show-cursor nil nil)
			(setq last-post-command-position (point))
			(add-hook 'post-command-hook 'fn-cleanup-after-highlight-current-match)

			(fn-setup-evil-ex-search)

			(return-from fn-evil-ex-search-apply)))
		(setq overlays (cdr overlays)))))
  )

(defun* fn-evil-ex-search (f)
  (interactive)
  (funcall f)
  (sit-for 0.02)
  (fn-evil-ex-search-apply)
  )

(define-key evil-motion-state-map (kbd "n") (lambda () (interactive)
												   (fn-evil-ex-search 'evil-ex-search-next)))
(define-key evil-motion-state-map (kbd "N") (lambda () (interactive)
											  (fn-evil-ex-search 'evil-ex-search-previous)))

(defun evil-ex-search-hook (f &rest args)
  (add-hook 'post-command-hook 'evil-ex-search-minibuffer-check)
  (apply f args)
  )

(defun evil-ex-search-minibuffer-check ()
  "Checks if focus still in minibuffer. Soon as it isn't, removes
hook, sets up fn-evil-search-ex stuff."
  (when (not (active-minibuffer-window))
	(remove-hook 'post-command-hook 'evil-ex-search-minibuffer-check)
	(sit-for 0.02)
	(fn-evil-ex-search-apply)
	)
  )

(advice-add 'evil-ex-search-forward :around 'evil-ex-search-hook)
(advice-add 'evil-ex-search-backward :around 'evil-ex-search-hook)
(setq evil-move-cursor-back nil)
;;; markers
(evil-visual-mark-mode -1)

(advice-add 'evil-delete-marks :after
			(lambda (&rest args)
			  (evil-visual-mark-render)))

(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;; (define-key evil-normal-state-map (kbd "C-j") 'open-line)
;; (define-key evil-normal-state-map (kbd "C-S-j") 'electric-newline-and-maybe-indent)
;; (define-key evil-normal-state-map (kbd "C-k") 'evil-delete-whole-line)

(define-key evil-inner-text-objects-map "c" 'evil-textobj-column-word)
(define-key evil-inner-text-objects-map "C" 'evil-textobj-column-WORD)
;; evil mode for customize
(evil-set-initial-state 'Custom-mode 'normal)
(setq evil-default-state 'normal)
;; fix pasting stuff in visual mode
(fset 'evil-visual-update-x-selection 'ignore)

;; -----------------------------------------------------------------------------
;;        new evil motions (wow)
;; -----------------------------------------------------------------------------

;; (define-key evil-motion-state-map (kbd "p") #'avy-goto-word-1)
;; (define-key evil-motion-state-map (kbd "P") #'avy-goto-line)
;; (define-key evil-visual-state-map (kbd "p") #'evil-paste-after)

(evil-set-initial-state 'dired-mode 'emacs)
;; (evil-set-initial-state ')

;; -> evil config

;; -- move cursor back to where we were before on stopping entry
(defun evil-ex-find-next-wrap (f &rest args)
  (remove-hook 'minibuffer-setup-hook 'evil-collection-minibuffer-insert)
  (run-with-idle-timer 0.1 nil (lambda () (interactive)
								 (add-hook 'minibuffer-setup-hook 'evil-collection-minibuffer-insert)))
  (save-excursion
	(apply f args)))
(advice-add 'evil-ex-find-next :around #'evil-ex-find-next-wrap)

;; -- prevent cursor from moving on search-string entry
(defun evil-ex-search-update-wrap (f &rest args)
  (save-mark-and-excursion
	(apply f args)))
(advice-add 'evil-ex-search-update :around #'evil-ex-search-update-wrap)
(advice-add 'evil-ex-search-full-pattern :around #'evil-ex-search-update-wrap)

;; ; no evil-mode in minibuffer for evil-ex-searches (on / and ?)
(defun evil-ex-start-search-wrap (f &rest args)
  (remove-hook 'minibuffer-setup-hook 'evil-collection-minibuffer-insert)
  (run-with-idle-timer 0.1 nil (lambda () (interactive)
								 (add-hook 'minibuffer-setup-hook 'evil-collection-minibuffer-insert)))
  (apply f args))

(advice-add 'evil-ex-start-search :around #'evil-ex-start-search-wrap)

;; TODO:  also temporarily adjust the max number of allowed lines between cursor
;;        and the top and bottom of the window?


;; -----------------------------------------------------------------------------
;;        WIP: text objects, sentence navigation
;; -----------------------------------------------------------------------------

;; (evil-define-text-object rsb/textobj-inner-c-defun (count &optional beg end type)
;;   (save-excursion
;;     (mark-defun)
;;     (re-search-forward "{")
;;     (exchange-point-and-mark)
;;     (re-search-backward "}")
;;     (evil-range (region-beginning) (region-end) type :expanded t)))

;; (evil-define-text-object rsb/textobj-outer-c-defun (count &optional beg end type)
;;   :type line
;;   (save-excursion
;;     (mark-defun)
;;     (if (looking-at "[:space:]*$")
;;         (forward-line))
;;     (exchange-point-and-mark)
;;     (unless (save-excursion
;;               (forward-line)
;;               (looking-at "[:space:]*$"))
;;       (forward-line))
;;     (evil-range (region-beginning) (region-end) type :expanded t)))

(evil-define-operator +evil-surround-region (beg end type char)
  "Like +evil-surround-region, but does not put delimiters on their own line
for lines selected in visual-mode."
  (interactive (evil-surround-input-region-char))
  (when (evil-surround-valid-char-p char)
    (let* ((overlay (make-overlay beg end nil nil t))
           (pair (or (and (boundp 'pair) pair) (evil-surround-pair char)))
           (open (car pair))
           (close (cdr pair))
           (beg-pos (overlay-start overlay)))
      (unwind-protect
          (progn
            (goto-char beg-pos)
            (cond ((eq type 'block)
                   (evil-surround-block beg end char))
                  ((eq type 'line)
                   (back-to-indentation)
                   (setq beg-pos (point))
                   (insert open)
                   (goto-char (- (overlay-end overlay) 1))
                   (insert close))
                  (t
                   (insert open)
                   (goto-char (overlay-end overlay))
                   (insert close)))
            (goto-char beg-pos))
        (delete-overlay overlay)))))

(evil-define-key 'visual evil-surround-mode-map (kbd "s") #'+evil-surround-region )
