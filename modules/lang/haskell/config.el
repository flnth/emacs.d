;; modules/lang/haskell/config.el    -*- lexical-binding: t; -*-




(with-eval-after-load 'intero
  (define-key intero-repl-mode-map (kbd "C-n") #'comint-next-input)
  (define-key intero-repl-mode-map (kbd "C-p") #'comint-previous-input))


;;;; haskell

;; -> module haskell

(require 'haskell-mode)
(require 'evil)

;; -- intero
(defun fn-load-hs-intero-repl ()
  "Loads the current file in the intero repl, and changes the working directory."
  (interactive)
  (let ((haskell-buffer (current-buffer))
		(working-directory default-directory))
	(intero-with-repl-buffer nil
							 (comint-simple-send
							  (get-buffer-process (current-buffer))
							  (concat ":cd " working-directory)))
	(set-buffer haskell-buffer)
	(intero-repl-load)
	)
  )

;; -- global haskell configuration
(defun fn-configure-haskell ()
  (interactive)
  (evil-define-key 'normal haskell-mode-map
	",sa" 'fn-load-hs-intero-repl
	",sb" 'fn-load-hs-intero-repl              ;; doesn't work :/
	(kbd "C-e") 'haskell-mode-jump-to-tag
	(kbd "C-S-e") 'xref-find-definitions-other-frame)

  (evil-define-key 'insert haskell-interactive-mode-map
	(kbd "<up>") 'haskell-interactive-mode-history-previous
	(kbd "<down>") 'haskell-interactive-mode-history-next)

  (add-to-list 'exec-path "~/.cabal/bin")  ;; TODO:  add cabal environment variable, somewhere, somehow

  (setq haskell-font-lock-symbols t)

  (set-face-foreground 'haskell-constructor-face "#cc6666")

  ;; default hoogle --info
  (setq haskell-hoogle-command "hoogle --info")
  )
(run-with-idle-timer 2 nil 'fn-configure-haskell)

;;-- buffer-local configuration
(add-hook 'haskell-mode-hook
		  (lambda () (progn
				  (setq tab-width 4)
				  (setq indent-tabs-mode nil)
				  (set (make-local-variable 'outline-regexp) "-- [*]\\{1,8\\} ")
				  (set (make-local-variable 'outline-promotion-headings ) '(("-- * " . 1)
																			("-- ** " . 2)
																			("-- *** " . 3)
																			("-- **** " . 4)
																			("-- ***** " . 5)
																			("-- ****** " . 6)
																			("-- ******* " . 7)
																			("-- ******** " . 8))))))

;; disable flycheck mode right after starting up intero...
;; (defun )

;; (spacemacs/set-leader-keys-for-major-mode 'haskell-mode
;;   (kbd "msa") 'fn-load-hs-intero-repl)

;; (defun intero-buffer-file-name (&optional buffer)
;;   "Call function `buffer-file-name' for BUFFER and clean its result.
;; The path returned is canonicalized and stripped of any text properties."
;;   (let ((name (buffer-file-name buffer)))
;;     (when name
;;       (concat "\""
;;               (intero-canonicalize-path (substring-no-properties name))
;;               "\""
;;               ))))


