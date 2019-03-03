;; modules/ui/code-navigation/config.el    -*- lexical-binding: t; -*-




;;;; xref  (largely unused atm, but module code-navigation?)
;; (defmacro after! (feature &rest forms)
;;   "A smart wrapper around `with-eval-after-load'. Supresses warnings during
;; compilation."
;;   (declare (indent defun) (debug t))
;;   `(,(if (or (not (bound-and-true-p byte-compile-current-file))
;;              (if (symbolp feature)
;;                  (require feature nil :no-error)
;;                (load feature :no-message :no-error)))
;;          #'progn
;;        #'with-no-warnings)
;;     (with-eval-after-load ',feature ,@forms)))

;; (after! xref
;;   (defun doom*xref-follow-and-close (orig-fn &rest args)
;;     "Jump to the xref on the current line, select its window and close the popup
;; you came from."
;;     (interactive)
;;     (let ((popup-p (doom-popup-p))
;;           (window (selected-window)))
;;       (apply orig-fn args)
;;       (when popup-p (doom/popup-close window))))
;; (advice-add #'xref-goto-xref :around #'doom*xref-follow-and-close))

(defun fn-xref-goto-xref ()
    "Follow the target under cursor, and close the xref window."
  (interactive)
  (let ((xref-jump-window (selected-window)))
    (xref-goto-xref)
    (recenter)
    (delete-window xref-jump-window)))


(defun fn-xref-peek ()
  "Peek at location under cursor, keep focus, restore when quit."
  (interactive)
  (xref--show-location (xref-item-location (xref--item-at-point))
                       nil))

(defun fn-xref-quit ()
  "Quit xref window, restore stored location information if it exists."
  (interactive)
  (delete-window)
  (xref-pop-marker-stack)
  (recenter))

(define-key xref--button-map (kbd "<RET>") 'fn-xref-goto-xref)
(define-key xref--button-map (kbd "TAB") 'fn-xref-peek)
(define-key xref--button-map (kbd "q") 'fn-xref-quit)
(define-key evil-motion-state-map "gm" 'counsel-imenu)

;;;; imenu

;; -> module code navigation, maybe

(defun goto-closest-imenu-item (direction)
  "Jump to the closest imenu item on the current buffer.
   If direction is 1, jump to next imenu item.
   If direction is -1, jump to previous imenu item.
   See https://emacs.stackexchange.com/questions/30673
   Adapted from `which-function' in https://github.com/typester/emacs/blob/master/lisp/progmodes/which-func.el"
  (let ((alist imenu--index-alist)
        (minoffset (point-max))
        offset pair mark imstack destination)
    ;; Elements of alist are either ("name" . marker), or
    ;; ("submenu" ("name" . marker) ... ). The list can be
    ;; arbitrarily nested.
    (while (or alist imstack)
      (if alist
          (progn
            (setq pair (car-safe alist)
                  alist (cdr-safe alist))
            (cond ((atom pair))			; skip anything not a cons
                  ((imenu--subalist-p pair)
                   (setq imstack   (cons alist imstack)
                         alist     (cdr pair)))
                  ((number-or-marker-p (setq mark (cdr pair)))
                   (if (> (setq offset (* (- mark (point)) direction)) 0)
                       (if (< offset minoffset)	; find the closest item
                           (setq minoffset offset
                                 destination mark))))))
        (setq alist     (car imstack)
              imstack   (cdr imstack))))
    (when destination (imenu-default-goto-function "" destination ""))))


;; imenu:  pulse after jump
(add-hook 'imenu-after-jump-hook (lambda () (interactive) (pulse-momentary-highlight-one-line (point))))


;; -- jumping -----

;; -> code-navigation, all of it (!)

;; NOTE: unusedd?
(defun fn-dumb-jump-search-in-directory (&optional fpath query)
  "Search for query in directory and subdirectories of file
fpath."
  (interactive)
  (let* ((fpath (if (null fpath) default-directory fpath ))
		 (search-dir (file-name-directory fpath))
		 (query (if (null query) (dumb-jump-get-point-symbol) query))
		 (info 	(dumb-jump-fetch-results fpath
										 search-dir
										 "c++"
										 "Makefile"
										 query))
		 (results (plist-get info :results))
		 (result-count (length results))
		 )
	(cond
	 ((= result-count 1)
	  (dumb-jump-result-follow (car results) nil search-dir))
	 ((> result-count 1)
	  (dumb-jump-handle-results results (plist-get info :file) search-dir (plist-get info :ctx-type)
								query nil nil))
	 )
	)
  )

;; -> c++
(defun fn-find-other-file (&optional fpath)
  "Find first matching corresponding header/source to fpath if it exists,
and return it."
  (interactive)
  (let* ((fpath (if (null fpath) (buffer-file-name) fpath))
		 (exts (cond ((seq-contains '("hpp" "h") (file-name-extension fpath)) '("c" "cpp"))
					 ((seq-contains '("c" "cc" "cpp") (file-name-extension fpath)) '("h" "hpp") )
					 (t "")))
		 (base-fpath (f-base fpath))
		 (other-fpath
		  (cl-loop for ext in exts do
				   (let ((other-fpath (concat base-fpath "." ext)))
					 (when (f-exists? other-fpath)
					   (return other-fpath))
					 )))
		 )
	other-fpath))

(defun fn-imenu-search-in-cpp (&optional current-file query)
  "Search in cpp file for query. "
  (interactive)
  (let* ((current-file (if (null current-file) (buffer-file-name) current-file))
		 (-compare-fn 'string=)
		 )
	(when (-contains? '("h" "hpp") (file-name-extension current-file))
	  (let* ((query (if (null query) (dumb-jump-get-point-symbol) query))
			 (other-file (fn-find-other-file current-file))
			 (semantic-index (save-window-excursion
							   (find-file other-file)
							   (imenu--make-index-alist t))))
		(cl-loop for (sym . marker) in semantic-index do
				 (when (s-suffix? query sym)
				   (xref-push-marker-stack)
				   (switch-to-buffer (marker-buffer marker))
				   (goto-char (marker-position marker))
				   (return)))))))

;; TODO:  imenu-search-in-h for e.g. files written in C or something?

(require 'smart-jump)
(smart-jump-register :modes '(c-mode c++-mode)
					 :jump-fn 'fn-imenu-search-in-cpp
					 :pop-fn nil
					 :heuristic 'point
					 :async nil
					 :order 1)
(smart-jump-register :modes '(c-mode c++-mode)
					 :jump-fn 'ycmd-goto
					 :refs-fn 'ycmd-goto-references
					 :heuristic 'point
					 :async 2000
					 :order 2)
(condition-case nil
	(progn 	;; NOTE: unuseable for this time being... -> just bound jump-to-def
	  (smart-jump-register :modes '(emacs-lisp-mode)
						   :jump-fn 'spacemacs/jump-to-definition
						   :refs-fn nil
						   :heuristic 'point
						   :async nil
						   :order 1))
  (error t))
(condition-case nil
	(progn	;; NOTE : dito.
	  (smart-jump-register :modes '(lisp-mode)
						   :jump-fn 'slime-edit-definition
						   :refs-fn nil
						   :heuristic 'point
						   :async nil
						   :order 2
						   ))
  (error t))

(define-key emacs-lisp-mode-map (kbd "M-.") 'spacemacs/jump-to-definition)
(evil-define-key '(normal insert visual) emacs-lisp-mode-map (kbd "M-.") 'spacemacs/jump-to-definition)

;; -> module code-navigation (!)
(defmacro fn--change-window-switch-to-buffer (action dir)
  "Constructs function that will perform ACTION in direction DIR
that is suitable for :around advising switch-to-buffer. Arguments
are evaluated on call, so it is possible to pass e.g. symbols."
  `(lambda (f buf &rest args)
	 ,(pcase (list (eval action) (eval dir))
		;; -------------------------------------------------
		('(split  left)  '(split-window-left-gui) )
		('(split  right) '(split-window-right-and-focus) )
		('(split  up)    '(split-window-above-gui) )
		('(split  down)  '(split-window-below-and-focus) )
		;; -------------------------------------------------
		('(select left)  '(windmove-left 1) )  ; relative to top-edge
		('(select right) '(windmove-right 1) ) ; -- " --
		('(select up)    '(windmove-up 1) )	   ; relative to left-edge
		('(select down)  '(windmove-down 1) )) ; -- " --
	 ;; -------------------------------------------------
	 (apply f buf args)))

(defvar fn--jump-to-def-next-func nil)

(defmacro fn--jump-to-definition-other-window (jump-fn)
  "Constructs function that determines in what window to
call jump-fn for jump-to-definition."
  `(lambda ()
	 (interactive)
	 (let ((split-window-switch-buffer-fn
			(ignore-errors
			  (if (window-in-direction 'right)
				  ,(fn--change-window-switch-to-buffer 'select 'right)
				(if (>= (window-width) 160 )
					,(fn--change-window-switch-to-buffer 'split 'right)
				  (cond ((window-in-direction 'below) ,(fn--change-window-switch-to-buffer 'select 'down))
						((window-in-direction 'above) ,(fn--change-window-switch-to-buffer 'select 'up))
						(t ,(fn--change-window-switch-to-buffer 'split 'down))
						))))))

	   ;; to be 100% sure:   remove old advice before adding new one
	   (advice-remove #'pop-to-buffer fn--jump-to-def-next-func)
	   (advice-remove #'switch-to-buffer fn--jump-to-def-next-func)

	   ;; for asynchronous jump-to-definition:
	   ;;   - store split-window-switch-buffer-fn
	   ;;   - use in the async handler's callback
	   (setf fn--jump-to-def-next-func split-window-switch-buffer-fn)

	   ;; for synchronous jump-to-definition:  add advice, jump, remove advice
	   (advice-add #'switch-to-buffer :around split-window-switch-buffer-fn)
	   (funcall ,jump-fn)
	   (advice-remove #'switch-to-buffer split-window-switch-buffer-fn)
	   )))

(defun fn--jump-to-definition-cleanup (&rest args)
  "To rule out the possibility of any errors: remove old advice
before any new calls to jump-to-definition."
  ;; to be 100% sure:   remove old advice
  (message "ycmd exception, cleaning up...")
  (advice-remove #'pop-to-buffer fn--jump-to-def-next-func)
  (advice-remove #'switch-to-buffer fn--jump-to-def-next-func)
  (setf fn--jump-to-def-next-func nil)
  )
(add-hook 'ycmd-after-exception-hook 'fn--jump-to-definition-cleanup)

(define-key emacs-lisp-mode-map (kbd "M->") (fn--jump-to-definition-other-window #'spacemacs/jump-to-definition) )
(define-key c++-mode-map (kbd "M->") (fn--jump-to-definition-other-window #'smart-jump-go) )

;; ycmd asynchronous callback instrumentation:
;; 1) for the result
(defun fn--ycmd-handle-goto-response-other-window (f &rest args)
  ;; ...called for non-other-window requests, too!
  (when fn--jump-to-def-next-func
	(advice-add #'switch-to-buffer :around fn--jump-to-def-next-func)
	(advice-add #'pop-to-buffer :around fn--jump-to-def-next-func))

  (let ((reset-fun (lambda ()
					 (when fn--jump-to-def-next-func
					   (advice-remove #'switch-to-buffer fn--jump-to-def-next-func)
					   (advice-remove #'pop-to-buffer fn--jump-to-def-next-func)
					   (setf fn--jump-to-def-next-func nil)
					   ))))
	(ignore-errors
	  (without-purpose
		(apply f args))
	  (funcall reset-fun)
	  (run-with-timer 0.5 nil reset-fun))))

(advice-add 'ycmd--handle-goto-response :around #'fn--ycmd-handle-goto-response-other-window)

(defun fn--ycmd-handle-message-response (f &rest args)
  "Wrapper around ycmd--handle-fixit-response,
ycmd--handle-message-response, and
ycmd--handle-detailed-info-response"
  (advice-remove #'switch-to-buffer fn--jump-to-def-next-func)
  (advice-remove #'pop-to-buffer fn--jump-to-def-next-func)
  (setf fn--jump-to-def-next-func nil)
  (apply f args))

(advice-add 'ycmd--handle-fixit-response :around #'ycmd-handle-message-response)
(advice-add 'ycmd--handle-message-response :around #'ycmd-handle-message-response)
(advice-add 'ycmd--handle-detailed-info-response :around #'ycmd-handle-message-response)


;; TODO: put to other utilities somewhere sane
(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))


;; -> code-navigation

;; NOTE: unused?
(defun fn-format-tag-prototype (tag &optional parent color)
  "Method for returning a prototype for semantic TAG.
(MOD) Includes class name as part of the name."
  (let* ((class (semantic-tag-class tag))
		 (name (semantic-format-tag-canonical-name tag parent color))
		 (type (if (member class '(function variable type))
				   (semantic-format-tag-type tag color)))
		 (args (if (member class '(function type))
				   (semantic--format-tag-arguments
					(if (eq class 'function)
						(semantic-tag-function-arguments tag)
					  (list "")
					  ;;(semantic-tag-type-members tag)
					  )
					#'semantic-format-tag-prototype
					color)))
		 (const (semantic-tag-get-attribute tag :constant-flag))
		 (tm (semantic-tag-get-attribute tag :typemodifiers))
		 (mods (append
				(if const '("const") nil)
				(cond ((stringp tm) (list tm))
					  ((consp tm) tm)
					  (t nil))
				))
		 (array (if (eq class 'variable)
					(let ((deref
						   (semantic-tag-get-attribute
							tag :dereference))
						  (r ""))
					  (while (and deref (/= deref 0))
						(setq r (concat r "[]")
							  deref (1- deref)))
					  r)))
		 (default (when (eq class 'variable)
					(let ((defval
							(semantic-tag-get-attribute tag :default-value)))
					  (when (and defval (stringp defval))
						(concat "[=" defval "]")))))
		 )
	(if args
		(setq args
			  (concat " "
					  (if (eq class 'type) "{" "(")
					  args
					  (if (eq class 'type) "}" ")"))))
	(when mods
	  (setq mods (concat (mapconcat 'identity mods " ") " ")))
	(concat (or mods "")
			(if type (concat type " "))
			name
			(or args "")
			(or array "")
			(or default ""))))

;;
;; (define-mode-local-override semantic-format-tag-prototype c++-mode (tag &optional parent color)
;;   "Override semantic-format-tag-prototype in C++-mode to display
;;   class-name before method-name"
;;   (fn-format-tag-prototype tag parent color)
;;   )


;; -- fix for imenu-c++-generic-expression (tabs!)

;; -> code-navigation

(setq cc-imenu-c++-generic-expression
	  `(
		;; Try to match ::operator definitions first. Otherwise `X::operator new ()'
		;; will be incorrectly recognized as function `new ()' because the regexps
		;; work by backtracking from the end of the definition.
		(nil
		 ,(concat
		   "^\\<.*"
		   "[^" c-alnum "_:<>~]"		; match any non-identifier char
										; (note: this can be `\n')
		   "\\("
		   "\\([" c-alnum "_:<>~]*::\\)?" ; match an operator
		   "operator\\>[ \t]*"
		   "\\(()\\|[^(]*\\)"			; special case for `()' operator
		   "\\)"

		   "[ \t]*([^)]*)[ \t]*[^ \t;]"	; followed by ws, arg list,
										; require something other than
										; a `;' after the (...) to
										; avoid prototypes.  Can't
										; catch cases with () inside
										; the parentheses surrounding
										; the parameters.  e.g.:
										; `int foo(int a=bar()) {...}'
		   ) 1)
		;; Special case to match a line like `main() {}'
		;; e.g. no return type, not even on the previous line.
		(nil
		 ,(concat
		   "^[ \t]*"
		   "\\([" c-alpha "_][" c-alnum "_:<>~]*\\)" ; match function name
		   "[ \t]*("								 ; see above, BUT
		   "[ \t]*\\([^ \t(*][^)]*\\)?)" ; the arg list must not start
		   "[ \t]*[^ \t;(]"				 ; with an asterisk or parentheses
		   ) 1)
		;; General function name regexp
		(nil
		 ,(concat
		   "^[ \t]*\\<"		  ; line MUST start with word char
		   ;; \n added to prevent overflow in regexp matcher.
		   ;; http://lists.gnu.org/archive/html/emacs-pretest-bug/2007-02/msg00021.html
		   "[^()\n]*"					; no parentheses before
		   "[^" c-alnum "_:<>~]"		; match any non-identifier char
		   "\\([" c-alpha "_][" c-alnum "_:<>~]*\\)" ; match function name
		   "\\([ \t\n]\\|\\\\\n\\)*("	 ; see above, BUT the arg list
		   "\\([ \t\n]\\|\\\\\n\\)*"	 ; must not start
		   "\\([^ \t\n(*]"				 ; with an asterisk or parentheses
		   "[^()]*\\(([^()]*)[^()]*\\)*" ; Maybe function pointer arguments
		   "\\)?)"
		   "\\([ \t\n]\\|\\\\\n\\)*[^ \t\n;(]"
		   ) 1)
		;; Special case for definitions using phony prototype macros like:
		;; `int main _PROTO( (int argc,char *argv[]) )'.
		;; This case is only included if cc-imenu-c-prototype-macro-regexp is set.
		;; Only supported in c-code, so no `:<>~' chars in function name!
		,@(if cc-imenu-c-prototype-macro-regexp
			  `((nil
				 ,(concat
				   "^[ \t]*\\<.*"		; line MUST start with word char
				   "[^" c-alnum "_]"	; match any non-identifier char
				   "\\([" c-alpha "_][" c-alnum "_]*\\)" ; match function name
				   "[ \t]*"				; whitespace before macro name
				   cc-imenu-c-prototype-macro-regexp
				   "[ \t]*("			; ws followed by first paren.
				   "[ \t]*([^)]*)[ \t]*)[ \t]*[^ \t;]" ; see above
				   ) 1)))
		;; Class definitions
		("Class"
		 ,(concat
		   "^[ \t]*"							; beginning of line is required
		   "\\(template[ \t]*<[^>]+>[ \t]*\\)?" ; there may be a `template <...>'
		   "\\(class\\|struct\\)[ \t]+"
		   "\\("						; the string we want to get
		   "[" c-alnum "_]+"			; class name
		   "\\(<[^>]+>\\)?"				; possibly explicitly specialized
		   "\\)"
		   "\\([ \t\n]\\|\\\\\n\\)*[:{]"
		   ) 3))
	  )


;;;; ycmd

(defun fn-load-ycmd ()
  (interactive)
  (use-package ycmd)
  (use-package company)
  (use-package ycmd-eldoc)
  (company-ycmd-setup)

  (setq ycmd-global-config (concat dir_stackroot "etc/ycmd/ycm_extra_conf.py") )
  (setq ycmd-server-command (list "python3" (concat dir_stackroot "opt/ycmd/ycmd") ))

  (add-hook 'c++-mode-hook 'ycmd-mode)
  (add-hook 'c++-mode-hook
			(lambda ()
			  (ycmd-eldoc-mode 1)
			  (make-local-variable 'company-backends)
			  (setq company-backends  (list 'company-ycmd 'company-files))
			  )
			)
  ;; make eldoc-mode not screw my tty cursor
  ;; TODO: move somewhere appropriate
  (defun update-tty-cursor-on-eldoc-message (&optional string)
	(+tty-update-cursor))
  (advice-add #'eldoc-message :after #'update-tty-cursor-on-eldoc-message)
  )

(run-with-idle-timer 2 nil 'fn-load-ycmd)

