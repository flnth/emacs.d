;; modules/ui/window-navigation/config.el    -*- lexical-binding: t; -*-


;;;; golden-ratio zooming

(custom-set-variables
 '(zoom-size '(0.618 . 0.618))
 '(zoom-ignored-major-modes '(inferior-emacs-lisp-mode) )
 '(zoom-ignored-buffer-names '("*ielm*"))
 )

;; (require 'zoom)

;; (defun +zoom-update ()
;;   "Update the window layout in the current frame."
;;   (interactive)
;;   (let ((zoom-mode nil)
;;         (window-configuration-change-hook nil)
;;         (window-combination-resize t)
;;         (window-resize-pixelwise t))
;;     ;; check if the selected window is not ignored
;;     (unless (zoom--window-ignored-p)
;;       (zoom--resize)
;;       (zoom--fix-scroll))))

;; (spacemacs/set-leader-keys "ww" #'+zoom-update)

;;;; screen -line utilities ()
;; ...for navigating from A -> B, and having cursor position on screen the same
;; before and after...

;; TODO:  when more navigation utilities, package. else put to e.g. org-rifle

(defun fn-point-at-N-screen-lines (N)
  "Calculate point at the position N screen lines away from
point. Move upwards if N bigger 0, otherwise move downwards."
  (save-excursion
	(vertical-motion (- N))
	(point)))

(defun fn-position-point-in-window (target-screen-line &optional point)
  "Scrolls buffer in current window such that point is at
target-screen-line."
  (prin1 target-screen-line)
  (let ((point (if (null point) (point) point)))
	(goto-char point)
	(set-window-start (selected-window)
					   (save-excursion
						 (vertical-motion (- target-screen-line))
						 (point)) t)))

(defun fn-point-to-screen-line (p)
  (count-screen-lines (window-start) (point)))

(defun fn-point-to-line (p) (line-number-at-pos p))

(defun fn-line-to-point (l)
  (save-excursion
	(goto-line l)
	(line-beginning-position)))
(global-set-key (kbd "ESC M-O q") (lambda nil (interactive) (tmux-create-pane "left")  ))
(global-set-key (kbd "ESC <kp-1>") (lambda nil (interactive) (tmux-create-pane "left")  ))
(global-set-key (kbd "ESC M-O r") (lambda nil (interactive) (tmux-create-pane "right")  ))
(global-set-key (kbd "ESC M-O s") (lambda nil (interactive) (tmux-create-pane "down")  ))
(global-set-key (kbd "ESC <kp-3>") (lambda nil (interactive) (tmux-create-pane "down")  ))
(global-set-key (kbd "ESC M-O t") (lambda nil (interactive) (tmux-create-pane "up")  ))

;; -------- WIP:  move point on minibuffer popup ----------------

;; -> module window management

;; (defun move-point-on-minibuffer-popup ()
;;   (let ((live-window (get-mru-window))
;; 		(minibuf (selected-window)))
;; 	(select-window live-window)
;; 	(make-local-variable 'point-before-minibuffer-popup)
;; 	(setq point-before-minibuffer-popup (point))
;; 	(move-to-window-line scroll-margin)
;; 	(select-window minibuf)
;;   ))

;; (add-hook 'minibuffer-setup-hook 'move-point-on-minibuffer-popup)
;; (remove-hook 'minibuffer-setup-hook 'move-point-on-minibuffer-popup)

;; (setq helm-display-function 'helm-display-buffer-popup-frame
;; 	  helm-display-buffer-reuse-frame t
;; 	  helm-use-undecorated-frame-option t
;; 	  )

;; (with-helm-in-frame
;;   (helm :sources (helm-build-sync-source "test"
;; 				   :candidates '("foo" "bar" "baz"))
;; 		:buffer "*helm test*"
;; 		))

;; (setq helm-display-function 'spacemacs//display-helm-window)
;; (defun ivy-read-wrap (f &rest args)
;;   (remove-hook 'minibuffer-setup-hook 'evil-collection-minibuffer-insert)
;;   (run-with-idle-timer 0.1 nil (lambda () (interactive)
;; 								 (add-hook 'minibuffer-setup-hook 'evil-collection-minibuffer-insert)))
;;   (apply f args)
;;   )
;; (advice-add 'ivy-read :around 'ivy-read-wrap)


;; ---------


(require 'navigate)
(setq dirs
	  '((left 	. "left")
		(right 	. "right")
		(above	. "up")
		(below	. "down")
		))
(setq dirs-opposites
	  '((left . right)
		(right . left)
		(above . below)
		(below . above)))


;; TODO: make setting out of it?
(setq fn-dwim-window-nav-modes '(compilation-mode
								 inferior-python-mode
								 inferior-emacs-lisp-mode
								 help-mode
								 helpful-mode
								 eshell-mode
								 ))
;; tuple (dir window)
(setq fn-dwim-window-nav-state nil)

;; -> module navigate!
;; uses tmux-utilities, everything navigation-specific stuff

(defun fn-tmux-navigate (dir)
  "Wrapper around tmux-navigate to restore focus on the window
	that was last-active when moving back to direction where one
	came from."
  (interactive)

  ;; --- leaving a window with major-mode in LIST? --------
  (if (and (not (null (member major-mode fn-dwim-window-nav-modes)))
		   (not (null fn-dwim-window-nav-state))
		   (eq (car fn-dwim-window-nav-state) dir)
		   (window-valid-p (cadr fn-dwim-window-nav-state)))
	  ;; t --> select suitable window instead of usual one
	  (progn
		(select-window (cadr fn-dwim-window-nav-state))
		(setq fn-dwim-window-nav-state nil)
		)

	;; nil --> enter other window, store where we came from if window in LIST
	(let ((w (selected-window)))
	  (tmux-navigate (cdr (assoc dir dirs)))
	  (when (not (null (member major-mode fn-dwim-window-nav-modes)))
		(setq fn-dwim-window-nav-state (list (cdr (assoc dir dirs-opposites)) w))))
	))

(defun fn-navigate (dir)
  (interactive)
  (if (window-in-direction dir nil t nil nil nil)
	  (fn-tmux-navigate dir)
	(if (not (display-graphic-p))
		(let ((cmd (if (frame-parameter nil 'tmux) (export-tmux-socket-path-cmd) "" )))
		  (shell-command-to-string
		   (cond
			((eq dir 'left) (concat cmd  "$ZDOTDIR/.zsh/utils/xnav.sh L 0" ))
			((eq dir 'below) (concat cmd "$ZDOTDIR/.zsh/utils/xnav.sh D 0" ))
			((eq dir 'above) (concat cmd "$ZDOTDIR/.zsh/utils/xnav.sh U 0" ))
			((eq dir 'right) (concat cmd "$ZDOTDIR/.zsh/utils/xnav.sh R 0" )))))
	  (cond
	   ((eq dir 'left)	(shell-command-to-string "xmonadctl 1"))
	   ((eq dir 'below)	(shell-command-to-string "xmonadctl 2"))
	   ((eq dir 'above)	(shell-command-to-string "xmonadctl 3"))
	   ((eq dir 'right)	(shell-command-to-string "xmonadctl 4"))))
	))

(global-set-key (kbd "M-h") (lambda () (interactive) (fn-navigate 'left)))
(global-set-key (kbd "M-j") (lambda () (interactive) (fn-navigate 'below)))
(global-set-key (kbd "M-k") (lambda () (interactive) (fn-navigate 'above)))
(global-set-key (kbd "M-l") (lambda () (interactive) (fn-navigate 'right)))

(define-key evil-normal-state-map (kbd "M-h") (lambda () (interactive)  (fn-navigate 'left)))
(define-key evil-normal-state-map (kbd "M-j") (lambda () (interactive)  (fn-navigate 'below)))
(define-key evil-normal-state-map (kbd "M-k") (lambda () (interactive)  (fn-navigate 'above)))
(define-key evil-normal-state-map (kbd "M-l") (lambda () (interactive)  (fn-navigate 'right)))

(evil-define-key '(normal insert) ediff-mode-map (kbd "M-h") (lambda () (interactive) (fn-navigate 'left)))
(evil-define-key '(normal insert) ediff-mode-map (kbd "M-j") (lambda () (interactive) (fn-navigate 'below)))
(evil-define-key '(normal insert) ediff-mode-map (kbd "M-k") (lambda () (interactive) (fn-navigate 'above)))
(evil-define-key '(normal insert) ediff-mode-map (kbd "M-l") (lambda () (interactive) (fn-navigate 'right)))

(evil-define-key 'normal evil-org-mode-map
  (kbd "M-l") 'nil
  (kbd "M-h") 'nil
  (kbd "M-k") 'nil
  (kbd "M-j") 'nil
  "g," 'nil)

(evil-define-key 'insert evil-org-mode-map
  (kbd "M-l") 'nil
  (kbd "M-h") 'nil
  (kbd "M-k") 'nil
  (kbd "M-j") 'nil)

(evil-define-key 'visual evil-org-mode-map
  (kbd "M-l") 'nil
  (kbd "M-h") 'nil
  (kbd "M-k") 'nil
  (kbd "M-j") 'nil)

(evil-define-key 'motion evil-org-mode-map
  (kbd "M-l") 'nil
  (kbd "M-h") 'nil
  (kbd "M-k") 'nil
  (kbd "M-j") 'nil)

(require 'org-agenda)
(evil-define-key 'motion org-agenda-mode-map
  (kbd "M-l") 'nil
  (kbd "M-h") 'nil
  (kbd "M-k") 'nil
  (kbd "M-j") 'nil)

(evilified-state-evilify-map org-agenda-mode-map
  :mode org-agenda-mode
  :bindings
  (kbd "M-h") 'nil
  (kbd "M-j") 'nil
  (kbd "M-k") 'nil
  (kbd "M-l") 'nil)
