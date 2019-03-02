;; modules/ui/tty/config.el -*- lexical-binding: t; -*-

(use-package ui/tty/tty
  ;; :after (xterm debug edebug tmux)
  :demand
  :init
  ;; (require 'xterm)
  (require 'debug)
  (require 'edebug)
  (require 'ui/tmux/tmux)

  ;; :bind (
  ;; 		 )
  ;; :config

  )




;; https://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
;; I prefer using the "clipboard" selection (the one the
;; typically is used by c-c/c-v) before the primary selection
;; (that uses mouse-select/middle-button-click)
(setq x-select-enable-clipboard t)

(defun cut-function (text &optional push)
  ;; (if (equal window-system nil)
      (with-temp-buffer
	  	(set-text-properties 0 (length text) nil text)
	  	(insert (s-chop-suffix "\n" text))
		(call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input"))
	  ;; (gui-select-text text)
	  ;; )
	  )

(defun paste-function ()
  ;; (if (equal window-system nil)
  (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
	(unless (string= (car kill-ring) xsel-output)
	  xsel-output ))
  ;; (gui-selection-value)
  ;; )
  )

(setq interprogram-cut-function 'cut-function)
(setq interprogram-paste-function 'paste-function)
;; -> tty

(defun configure-tty-keys ()
  ;; line stuff
  (define-key input-decode-map "\e[1;6A" [(control shift n)])
  (define-key input-decode-map "\e[1;6B" [(control shift p)])
  (define-key input-decode-map "\e[1;6C" [(control shift b)])

  ;; more navigation stuff
  (define-key input-decode-map "\e[8;3A" [(control shift h)])
  (define-key input-decode-map "\e[7;3A" [(control shift j)])
  (define-key input-decode-map "\e[7;3D" [(control shift k)])
  (define-key input-decode-map "\e[8;3D" [(control shift l)])

  ;; accessing stuff
  (define-key input-decode-map "\e[2;1A" [(menu)])
  (define-key input-decode-map "\e[2;1B" [(meta menu)])
  (define-key input-decode-map "\e[2;1C" [(control menu)])
  (define-key input-decode-map "\e[2;1D" [(shift menu)])

  ;; buffer / frame movement
  (define-key input-decode-map "\e[5;1A" [(s up)])
  (define-key input-decode-map "\e[5;1B" [(s down)])
  (define-key input-decode-map "\e[5;1C" [(s left)])
  (define-key input-decode-map "\e[5;1D" [(s right)])

  ;; alt-up/down/left/right keys
  (define-key input-decode-map "\e[1;3A" [(meta up)])
  (define-key input-decode-map "\e[1;3B" [(meta down)])
  (define-key input-decode-map "\e[1;3D" [(meta left)])
  (define-key input-decode-map "\e[1;3C" [(meta right)])

  ;; S-M-up/down/left/right keys
  (define-key input-decode-map "\e[1;4A" [(shift meta up)])
  (define-key input-decode-map "\e[1;4B" [(shift meta down)])
  (define-key input-decode-map "\e[1;4D" [(shift meta left)])
  (define-key input-decode-map "\e[1;4C" [(shift meta right)])

  ;; folding
  (define-key input-decode-map "\e[3;1A" [(control tab)])
  (define-key input-decode-map "\033[9;1E" [(control meta tab)])
  (define-key input-decode-map "\033[9;2A" [(meta tab)])

  ;; home/end
  (define-key input-decode-map "\033[1;7F" [(end)])
  (define-key input-decode-map "\e[1;7G" [(control end)])
  (define-key input-decode-map "\033[1;7H" [(home)])
  (define-key input-decode-map "\e[1;7I" [(control home)])

  ;; saving
  (define-key input-decode-map "\e[1;A" [(control shift s)])

  ;; C-pgup/C-pgdown
  (define-key input-decode-map "\e[6;5~" [(control next)])
  (define-key input-decode-map "\e[5;5~" [(control prior)])

  ;; C-S-e,  C-S-t, C-S-b
  (define-key input-decode-map "\e[1;65" [(control shift e)])
  (define-key input-decode-map "\e[1;66" [(control shift t)])

  ;; C-, C-.
  (define-key input-decode-map "\e[3;1B" [(control comma)])
  (define-key input-decode-map "\e[3;1C" [(control period)])

  ;; terminal escape codes     (put somewhere obscure...)
  (define-key input-decode-map "\e[9;1B" [(control shift pause)] ) ; C-i
  (define-key input-decode-map "\e[9;1A" [(control shift pause)] ) ; C-I
  (define-key input-decode-map "\e[9;1C" [(control shift print)] ) ; C-[
  (define-key input-decode-map "\e[9;1D" [(control shift meta print)] ) ; C-S-M-print, C-S-[

  ;; mouse wheel
  ;; (define-key input-decode-map "\033[M'X1-" (kbd "<wheel-up>"))
  )
(configure-tty-keys)

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (configure-tty-keys)
            )
          )
