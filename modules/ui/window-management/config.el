;; modules/ui/window-management/config.el    -*- lexical-binding: t; -*-




;;; window purpose (module window-management?)
;; NOTE:  subject to big changes in doom --> prepare by putting everything in one spot

;; default action for when no display function in purpose-action-sequences fit
;; (seldomly used, if at all?)
;; (setq purpose-default-action-order 'force-same-window)

;; display actions:  user -> special -> normal -> default

;; purpose-user-action-sequences

;; specify action sequences for some buffers BEFORE trying others
;; purpose-special-action-sequences

;; -- helm-org-rifle-occur:  open in same window -------------
;; NOTE: no, do it via explicitly opening the thing in the same window.
;; (setq p purpose-special-action-sequences)

;; (defun helm-org-rifle-occur-condition (purpose buffer alist)
;;   "Return t if previous buffer was "
;;   (if (string= (buffer-name (other-buffer buffer 1)) "*helm-org-rifle-occur*")
;; 	  (progn
;; 		(prin1 "yes")
;; 		t
;; 		)
;; 	nil))

;; (defun helm-org-rifle-occur-display-buffer (buffer alist)
;;   )

;; (setq purpose-special-action-sequences '((pupo/display-condition pupo/display-function) (imenu-list-purpose-display-condition imenu-list-display-buffer)))

;; -- ert, do not let ert window steal focus -----

(defvar ert--window-pre nil)

(defun fn--ert-before (selector &optional output-buffer-name message-fn)
  (setf ert--window-pre (selected-window))
  )

(defun fn--ert-after (selector &optional output-buffer-name message-fn)
  (when (not (null ert--window-pre))
	(select-window ert--window-pre))
  )

(advice-add 'ert :before #'fn--ert-before)
(advice-add 'ert :after #'fn--ert-after)

;; (defun fn-ert-buffer-condition (purpose buffer alist)
;;   "Return true if buffer is an ert buffer."
;;   (with-current-buffer buffer
;; 	(princ major-mode))
;;   )

;; (add-to-list 'purpose-special-action-sequences '(fn-ert-buffer-condition display))


;; -- repl display ---------

;; (defun fn-repl-buffer-condition (purpose buffer alist)
;;   "Return true if buffer is a known repl buffer as specified in
;;   `fn-major-mode-repl-alist'"
;;   (cl-loop for repl-triplet in fn-major-mode-repl-alist do
;; 		   (when (string= (buffer-name (buffer)) (cadr fn-major-mode-repl-alist))
;; 			 (cl-return t)
;; 			 )
;; 		   )
;;   )

;; (add-to-list 'purpose-special)


;; (add-to-list 'purpose-special-action-sequences '(helm-org-rifle-occur-condition display-buffer-same-window))
;; (add-to-list 'purpose-special-action-sequences '(helm-org-rifle-occur-condition display-buffer-same-window))
;; -------------

;; popup windows:
;; By default, popwin not called for any buffer handled by purpose.
;;   - purpose sets display-buffer-overriding-action
;;   - popwin sets display-buffer-alsit
;; Make purpose ignore a buffer by using purpose-action-function-ignore-buffer-names

;; helm:
;; By default, purpose ignores helm buffers. Use helm-display-function

;; -- change other-window for it to always select MRU window instead of as per cyclic ordering
;; (defun other-window-mru (other-window COUNT &optional ALL-FRAMES)  (NOTE: crashes customize-read-group)
;;   (prin1 "-- OTHER-WINDOW-MRU!")
;;   (if (or (> (abs COUNT) 1)
;;   		  (not (null ALL-FRAMES)))
;;   	  (apply other-window COUNT ALL-FRAMES)
;;   	(select-window (get-mru-window nil t t)))
;;   )
;; (advice-add 'other-window :around #'other-window-mru)
;; (advice-remove 'other-window #'other-window-mru)

;; -- make purpose use the mru-used window among windows with a purpose instead
;;    the first best one.
(defun get-last-mru-window-among (windows &optional not-prefer-selected)
  "Return the most recently used window among WINDOWS. Ignores
the selected window if windows contains more than one window."
  (let (best-window best-time time)
	(if (> (length windows) 1)
		(progn
		  (dolist (window windows)
			(setq time (window-use-time window))
			(when (and  (or (not best-time)
							(> time best-time))
						(not (eq window (selected-window))))
			  (setq best-time time)
			  (setq best-window window)))
		  best-window)
	  (car windows))))

(defun fn-purpose-display-reuse-window-purpose (buffer alist &optional purpose)
  "Display BUFFER in a window that is already used for purpose PURPOSE.
Identical to purpose-display-reuse-window-purpose, but instead
chooses the MRU window among the candidates instead of the first
one (whatever the first one, randomly, may be)."
  (let-alist alist
    (let* ((frames (purpose--reusable-frames alist))
           (windows (purpose-flatten (mapcar #'window-list frames)))
           (purpose (or purpose (purpose-buffer-purpose buffer)))
           window)
      (setq windows (cl-delete-if-not
                     #'(lambda (window)
                         (purpose-window-purpose-reusable-p window purpose))
                     windows))
	  (when .inhibit-same-window
		(setq windows (delq (selected-window) windows)))
	  ;; (setq window (car windows))  ;; do NOT use the first best match, but the last-selected

	  ;; do NOT use the first best match, but the last-selected
	  (setq window (get-last-mru-window-among windows))

	  ;; do NOT use last-selected, but, if possible, last-selected with that major-mode

	  (when window
        (purpose-change-buffer buffer window 'reuse alist))
      window)))

(run-with-idle-timer 10 nil
					 '(lambda ()
						(setq purpose-action-sequences
							  (cl-loop for lst in purpose-action-sequences collect
									   (cl-substitute 'fn-purpose-display-reuse-window-purpose 'purpose-display-reuse-window-purpose lst)
									   )
							  )))
;; -> window management module

;; tmux-emacs integration:  M-hjkl to navigate panes in emacs and tmux
(with-eval-after-load "evil-maps"
  (require 'navigate))

(with-eval-after-load "org"
  (require 'navigate))

;; was:  C-x left/right and C-x C-left/right  to (next-buffer) and (previous buffer)
(global-set-key (kbd "C-x d") 'delete-window)

;; -- for GUI and terminal:  C-x prefix
(defun split-window-above-gui ()
  (interactive)
  (setq evil-split-window-below t)
  (evil-window-split)
  (windmove-up)
  )

(defun split-window-left-gui ()
  (interactive)
  (setq evil-vsplit-window-right t)
  (evil-window-vsplit)
  (windmove-left)
  )

;; -- resizing hydra --
(require 'move-border)
(defhydra hydra-window ()
  "^Resize^"
  ("M-h" (move-border-left 2))
  ("M-j" (move-border-down 2))
  ("M-k" (move-border-up 2))
  ("M-l" (move-border-right 2))
  ("SPC" nil)
  )

 ;; -- for terminal:  C-x prefix, same as in tmux C-a
(run-with-idle-timer 1 nil (lambda ()
							 (global-set-key (kbd "C-x C-y") 'spacemacs/toggle-maximize-buffer)
							 (global-set-key (kbd "C-x y")   'spacemacs/toggle-maximize-buffer)
							 (global-set-key (kbd "C-x C-h") 'split-window-left-gui)
							 (global-set-key (kbd "C-x C-j") 'split-window-below-and-focus)
							 (global-set-key (kbd "C-x C-k") 'split-window-above-gui)
							 (global-set-key (kbd "C-x C-l") 'split-window-right-and-focus)

							 (global-set-key (kbd "C-x M-h") 'hydra-window/lambda-M-h)
							 (global-set-key (kbd "C-x M-j") 'hydra-window/lambda-M-j)
							 (global-set-key (kbd "C-x M-k") 'hydra-window/lambda-M-k)
							 (global-set-key (kbd "C-x M-l") 'hydra-window/lambda-M-l)
							 )
 )

;; -- for GUI:  C-a prefix, same as in tmux
(define-prefix-command 'C-a-map)
(global-set-key (kbd "C-a") C-a-map)
(define-key C-a-map (kbd "C-a") 'mwim-beginning-of-code-or-line)
(define-key C-a-map (kbd "C-y") 'spacemacs/toggle-maximize-buffer)
(define-key C-a-map (kbd "y") 'spacemacs/toggle-maximize-buffer)
(define-key C-a-map (kbd "d") 'delete-window)
(define-key C-a-map (kbd "C-h") 'split-window-left-gui)
(define-key C-a-map (kbd "C-j") 'split-window-below-and-focus)
(define-key C-a-map (kbd "C-k") 'split-window-above-gui)
(define-key C-a-map (kbd "C-l") 'split-window-right-and-focus)

(define-key C-a-map (kbd "M-h") 'hydra-window/lambda-M-h)
(define-key C-a-map (kbd "M-j") 'hydra-window/lambda-M-j)
(define-key C-a-map (kbd "M-k") 'hydra-window/lambda-M-k)
(define-key C-a-map (kbd "M-l") 'hydra-window/lambda-M-l)

;; -- for GUI:
(global-set-key (kbd "M-t") 'eyebrowse-create-window-config)
(global-set-key (kbd "M-<next>") 'eyebrowse-next-window-config)
(global-set-key (kbd "M-<prior>") 'eyebrowse-prev-window-config)

(run-with-idle-timer 2 nil (lambda () (interactive)
							 (global-set-key (kbd "C-x C-j") 'split-window-below-and-focus)))
;;;;  window configurations

;; -> window configuration module, eyebrowse, etc.

;; close window configuration if the last window is the last window
(defun count-unique-visible-buffers (&optional frame)
  "Count how many buffers are currently being shown.  Defaults to
selected frame."
  (length (cl-delete-duplicates (mapcar #'window-buffer (window-list frame)))))

(defun count-visible-buffers (&optional frame)
  (length (mapcar #'window-buffer (window-list frame)))
  )

;; keymaps
(define-key global-map (kbd "C-S-t") 'eyebrowse-create-window-config)
(define-key global-map (kbd "C-x d") 'fn-close-window)
(define-key global-map (kbd "C-x <prior>") 'eyebrowse-prev-window-config)
(define-key global-map (kbd "C-x <next>") 'eyebrowse-next-window-config)
;;;; fix C-a/C-x bindings in certain major modes

;; -- eshell --
(defun eshell-C-a-map()
  (define-key eshell-mode-map (kbd "C-a") 'C-a-map)
  (define-key C-a-map (kbd "C-a") 'eshell-bol))

(add-hook 'eshell-mode-hook 'eshell-C-a-map)

;; (defun eshell-C-x-map()
;;   (define-key eshell-mode-map (kbd "C-x") 'C-x-map)
;;   (define-key C-x-map (kbd "C-x") 'eshell-bol))

;; (add-hook 'eshell-mode-hook 'eshell-C-x-map)

;; -- org mode --
(defun org-C-a-map ()
  (define-key org-mode-map (kbd "C-a") 'C-a-map)
  (define-key C-a-map (kbd "C-a") 'org-beginning-of-line))

(add-hook 'org-mode-hook 'org-C-a-map)

(evil-define-key '(normal visual) org-mode-map (kbd ",p")
  '(lambda () (interactive)
	 (fn-tty-hide-cursor)
	 (org-priority)
	 (run-with-timer 0.01 nil '(lambda ()
								 (fn-tty-show-cursor)))))

(spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode "p"
  '(lambda () (interactive)
	 (fn-tty-hide-cursor)
	 (org-agenda-priority)
	 (run-with-timer 0.01 nil '(lambda ()
								 (fn-tty-show-cursor)))))

(spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode "sA"
  #'org-agenda-archive)
;; popwin popup-window configuration
(push '("*pdb*" :regexp nil :height 0.2 :position bottom)
      popwin:special-display-config)
(push '("*Occur*" :regexp nil :height 0.2 :position bottom)
      popwin:special-display-config)

(push '("*Python*" :regexp nil :height 0.28 :position bottom)
      popwin:special-display-config)
