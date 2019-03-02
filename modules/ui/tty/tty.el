;;; tty.el                                            -*- lexical-binding: t -*-
;;
;; Filename: tty.el
;;
;; Copyright (C) 2018 Florian Thevißen
;;
;; Description: tty-related miscellenia
;; Author: Florian Thevißen <mail@florian-thevissen.de>
;; Keywords: lisp, tools
;; Created: Sun Oct 28 17:15:36 2018 (+0100)
;; Version: 0.1.0
;; Package-Required: TODO
;; URL: TODO
;;
;; -----------------------------------------------------------------------------
;;; Commentary
;;
;; tty configuration:   copy/paste, 
;;
;; -----------------------------------------------------------------------------
(message "loading ui/ui/tty ...")

;;; tty configuration

;; remove delay on emacsclient startup
(defadvice xterm--query (around tweak-for-gnu-screen (query handlers) activate)
  ;; GNU screen does not support this sequence
  (unless (string= query "\e]11;?\e\\")
	ad-do-it))

(setq-default xterm-query-timeout 1)

;; -- fix the mouse (for evil mode, in the terminal) --
(defun do-terminal-mouse-fix ()
  (define-key evil-motion-state-map[down-mouse-1] 'mouse-drag-region)
  (define-key evil-motion-state-map[mouse-1] 'mouse-set-point)
  (global-set-key (kbd "<down-mouse-1>") 'mouse-drag-region)
  (global-set-key (kbd "<mouse-1>") 'mouse-set-point))

;; (add-hook 'buffer-list-update-hook 'do-terminal-mouse-fix) ;fix needed whenever buffer changes...

;;; copy/paste

;; (defun tty-configure-copy-paste ()
;;   ;; (require 'mouse)
;;   ;; (xterm-mouse-mode t)
;;   ;; (defun track-mouse (e))
;;   ;; (setq mouse-sel-mode t)

;;   ;; enable clipboard in emacs
;;   (setq x-select-enable-clipboard t)

;;   ;; enable copy/paste between emacs and other apps (terminal version of emacs)
;;   (unless window-system
;;     (when (getenv "DISPLAY")
;;       ;; Callback for when user cuts
;;       (defun xsel-cut-function (text &optional push)
;;         ;; Insert text to temp-buffer, and "send" content to xsel stdin
;;         (with-temp-buffer
;;           (insert text)
;;           ;; I prefer using the "clipboard" selection (the one the
;;           ;; typically is used by c-c/c-v) before the primary selection
;;           ;; (that uses mouse-select/middle-button-click)
;;           (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
;;       ;; Call back for when user pastes
;;       (defun xsel-paste-function()
;;         ;; Find out what is current selection by xsel. If it is different
;;         ;; from the top of the kill-ring (car kill-ring), then return
;;         ;; it. Else, nil is returned, so whatever is in the top of the
;;         ;; kill-ring will be used.
;;         (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
;;           (unless (string= (car kill-ring) xsel-output)
;;             xsel-output )))
;;       ;; Attach callbacks to hooks
;;       (setq interprogram-cut-function 'xsel-cut-function)
;;       (setq interprogram-paste-function 'xsel-paste-function)
;;       ;; Idea from
;;       ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
;;       ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
;;       )))

;;; tty information in frame
;;  -- frame information
(add-hook 'after-make-frame-functions
		  (lambda (new-frame)
			(let ((current-frame (selected-frame)))
			  (select-frame new-frame)
			  (if (display-graphic-p)
				  (progn
					(set-frame-parameter new-frame 'tmux nil)
					(set-frame-parameter new-frame 'term nil))
				(progn
				  (set-frame-parameter new-frame 'tmux (+tmuxp))
				  (set-frame-parameter new-frame 'term t)
				  (turn-on-xterm-mouse-tracking-on-terminal (frame-terminal))))
			  (select-frame current-frame))
			))


;;; Cursor shape in tty/gui

;; disable blinking cursor
(setq visible-cursor nil)

;; set evil cursors in terminals --
(setq cursor-escape-codes
      '(
        ;; -- urxvt / tmux   (start with \ePtmux;\e\033,  end with \)
        (insert-entry-tmux . "\ePtmux;\e\033]12;#66cd00\007\e\033[6 q\e\\")
        (insert-exit-tmux  . "\ePtmux;\e\033]12;#ffaf00\007\e\033[2 q\e\\")
        (visual-entry-tmux . "\ePtmux;\e\033]12;#cd96cd\007\e\033[4 q\e\\")
        (visual-exit-tmux  .  "\ePtmux;\e\033]12;#ffaf00\007\e\033[2 q\e\\")
        (replace-entry-tmux . "\ePtmux;\e\033]12;#8b0000\007\e\033[4 q\e\\")
        (replace-exit-tmux  . "\ePtmux;\e\033]12;#ffaf00\007\e\033[2 q\e\\")
		(hide-cursor-tmux  .  "\ePtmux;\e\033]12;#1d1d1a\007\e\033[2 q\e\\")
        ;; -- urxvt
        (insert-exit . "\e[2 q\e \e]12;#ffaf00\007")
        (insert-entry . "\e[6 q\e \e]12;#66cd00\007")
        (visual-entry . "\e]12;#cd96cd\007 \e[4 q")
        (visual-exit  .  "\e]12;#ffaf00\007 \e[2 q")
        (replace-entry . "\e]12;#8b0000\007 \e[4 q" )
        (replace-exit  . "\e]12;#ffaf00\007 \e[2 q" )
        (hide-cursor . "\e]12;#1d1d1a\007 \e[2 q" )
        ))

(defun fn-tty-hide-cursor ()
  (interactive)
  (when (not (display-graphic-p))
	(send-escape-code-cond 'hide-cursor-tmux 'hide-cursor)))

(defun fn-tty-show-cursor ()
  (interactive)
  (when (not (display-graphic-p))
	(send-escape-code-cond 'insert-exit-tmux 'insert-exit)))

(defun send-escape-code-cond (e1 e2)
  (when (frame-parameter nil 'term)
    (if (frame-parameter nil 'tmux)
        (send-string-to-terminal (cdr (assoc e1 cursor-escape-codes)))
      (send-string-to-terminal (cdr (assoc e2 cursor-escape-codes))))))

;; TODO do state-based, e.g. something like
;;   change-terminal-state(state)
;;     if tmux -> else, bla

(add-hook 'evil-insert-state-entry-hook ( lambda () (send-escape-code-cond 'insert-entry-tmux
                                                                           'insert-entry)))
(add-hook 'evil-insert-state-exit-hook (lambda () (send-escape-code-cond 'insert-exit-tmux
                                                                         'insert-exit)))
(add-hook 'evil-visual-state-entry-hook (lambda () (send-escape-code-cond 'visual-entry-tmux
                                                                          'visual-entry)))
(add-hook 'evil-visual-state-exit-hook (lambda () (send-escape-code-cond 'visual-exit-tmux
                                                                         'visual-exit)))
(add-hook 'evil-replace-state-entry-hook (lambda () (send-escape-code-cond 'replace-entry-tmux
                                                                           'replace-entry)))
(add-hook 'evil-replace-state-exit-hook (lambda () (test-send-str-to-terminal 'replace-exit-tmux
                                                                              'replace-exit)))
;; change tty cursor depending on current evil-state
(defun +tty-update-cursor ()
  (interactive)
  (when (not (display-graphic-p))
	(cond
	 ((eq evil-state 'insert) (send-escape-code-cond 'insert-entry-tmux
													 'insert-entry) )
	 ((eq evil-state 'visual) (send-escape-code-cond 'visual-entry-tmux
													 'visual-entry) )
	 ((eq evil-state 'normal) (send-escape-code-cond 'insert-exit-tmux
													 'insert-exit) )
	 (t (send-escape-code-cond  'insert-exit-tmux
						  		'insert-exit))
	 ))
  )
(global-set-key (kbd "M-O x") '+tty-update-cursor)
(global-set-key (kbd "ESC M-O x") '+tty-update-cursor)

(add-hook 'change-major-mode-hook '+tty-update-cursor)
;; (defadvice select-window (after select-window-and-do-stuff activate)
;;   (+tty-update-cursor))

;; (defadvice top-level (&rest args) '+tty-update-cursor)
;; (define-key debugger-mode-map "q" (lambda () (top-level) (+tty-update-cursor))


;;;; debuggers:  force cursor-updates
;; NOTE: all of this was on run-with-idle-timer 2
(setq debugger-mode-map (delq (assoc 113 debugger-mode-map) debugger-mode-map))
;; (setq edebugger-mode-map (delq (assoc 113 inferior-python-mode-map) edebugger-mode-map))
(setq global-mode-map (delq (assoc 113 global-edebug-map) global-edebug-map))

(defun +tty-debugger-top-level-cursor-update ()
  (interactive)
  (run-with-idle-timer 0.01 nil (lambda () (+tty-update-cursor)))
  (top-level))

(define-key debugger-mode-map "q" '+tty-debugger-top-level-cursor-update)
(define-key global-edebug-map "q" '+tty-debugger-top-level-cursor-update)


(message "... done!")
(provide 'ui/tty/tty)
