;; modules/ui/prettify/config.el    -*- lexical-binding: t; -*-

(defun fn-prettify-symbols-python ()
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955)
          ("->" . 8594)
          ("<-" . 8592)
          ("=>" . 8658)
          ("<=" . 8656)
          ("<=>" . 8660)
          ;; ("map" . 8614)
          ("and" . 8743)
          ("or" . 8744)
          ("xor" . 8853)
          ;; ("all" . 8704)
          ;; ("ex" . 8707)
		  ("!=" . 8800)
		  (">=" . 8805)
		  ("<=" . 8804)
          ;; ("def" . 8788)
          ("con" . 8757)
          ))
  (prettify-symbols-mode 1))

(defun fn-prettify-symbols-lisp ()
  (interactive)
  (setq prettify-symbols-alist
  		'(
  		  ("lambda" . 955)
		  ("-lambda" . (?- (Br . Bl) 955))
  		  ("->" . (?- (Br . Bc) ?- (Br . Bc) ?>))
  		  ("->>" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s
  		  				 (Bl . Bl) ?- (Bc . Br) ?- (Bc . Bc) ?>
  		  				 (Bc . Bl) ?- (Br . Br) ?>))
  		  ("-->" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s
  		  				 (Bl . Bl) ?- (Bc . Br) ?- (Bc . Bc) ?-
  		  				 (Bc . Bl) ?- (Br . Br) ?>))
  		  ))
  (prettify-symbols-mode 1)
  )

(run-with-timer 10 nil (lambda ()
						 (add-hook 'emacs-lisp-mode-hook 'fn-prettify-symbols-lisp)
						 (add-hook 'inferior-emacs-lisp-mode-hook 'fn-prettify-symbols-lisp)))

(defun fn-prettify-symbols-org ()
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955)
          ("->" . 8594)
          ("<-" . 8592)
		  ("<->" . 8596)
		  ;; ("=>" . 8658)
          ;; ("<=" . 8656)
          ("<=>" . 8660)
          ("\\and" . 8743)
          ("\\or" . 8744)
          ("\\xor" . 8853)
          ("\\all" . 8704)
          ("\\ex" . 8707)
          ("\\def" . 8788)
          ("\\con" . 8757)
		  ("!=" . 8800)
		  (">=" . 8805)
		  ("<=" . 8804)
          ))
  (prettify-symbols-mode 1)
  (setq org-pretty-entities t)	; prettify-sybmol mode doesn't work in org-mode?
  )

(add-hook 'python-mode-hook 'fn-prettify-symbols-python)
(add-hook 'org-mode-hook 'fn-prettify-symbols-org)
