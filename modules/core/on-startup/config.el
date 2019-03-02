;; modules/core/on-startup/config.el    -*- lexical-binding: t; -*-

(require 'f)

;; ---- byte-compile modules after boot
(ignore-errors
  (let ((filename "/tmp/emacs_system_bytecompiled"))
	(when (not (f-exists-p filename))
	  (byte-recompile-directory (concat dir_emacs "modules") 0 t )
	  (f-append " " 'utf-8 filename))))

;; ---- auto-connect bitlbee at work
(when (and (string= (getenv "LOC") "work")
		   (+utils-authinfo-init-p))
  (run-with-timer 10 nil
				  (lambda ()
					(require 'erc)
					(+chat-irc-connect-bitlbee)
					(run-with-timer 20 nil
									(lambda ()
									  (+chat-irc-bitlbee-join-channels))))))

;; ---- auto-connect znc at home (freenode/OFTC)



;; write pid and wid into /tmp/emacsin g.fo for guake-like showing hiding
;; (defun write-emacsinfo ()
;;   (setq pid (number-to-string (emacs-pid)))
;;   (setq wid (shell-command-to-string
;;              (concat "xdotool search --all --pid " pid " --name \"duff\" ")))
;;   (setq wid (replace-regexp-in-string "\n\\'" "" wid))
;; =======
;; (defun write-emacsinfo ()
;;   (setq pid (number-to-string (emacs-pid)))
;;   (setq wid (shell-command-to-string
;;              (concat "xdotool search --all --pid " pid " --name \"fthevissen\" ")))
;;   (setq wid (replace-regexp-in-string "\n\\'" "" wid))

;;   (f-write-text (concat "\"" pid "\" \"" wid "\"") 'utf-8 "/tmp/emacsinfo")
;;   )

;; (add-hook 'window-setup-hook 'write-emacsinfo)


