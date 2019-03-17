;; modules/lang/cc/config.el    -*- lexical-binding: t; -*-


;; -----------------------------------------------------------------------------
;;;; C, C++
;; -----------------------------------------------------------------------------

(require 'cc-mode)

;; (spacemacs/toggle-semantic-stickyfunc-globally-on)
(with-eval-after-load 'semantic
  (global-semantic-stickyfunc-mode))


;; -- Compilation ----------------------------------
;; USE SPC-c-c to compile from anywhere within a project!
;; question:  how to compile when you're within an external library?  (i.e. the iframework, ...?)


;; a (default-) path relative to the projectile project root where a make file can be found
(setq-default helm-make-build-dir "build")

;; possibility to set buffer-local variables to all files below a certain directory
;; via creation of a .dir-locals.el in that directory, and then putting something in like
;;    ((c++-mode (helm-make-build-dir . "build)))
;; IF this works, one is upon opening a file asked whether the association is safe

;; add this variable to a safe-variable list, so you dont get prompted all the time, ..
(put 'helm-make-build-dir 'safe-local-variable 'stringp)

;; -- file jumping ------------------------
;; add more file-types to other-file
(with-eval-after-load 'projectile
  (push '("C" "h") projectile-other-file-alist))

;; find-other-file:   prefer files in the same directory
;; (defun fn-find-other-file ()
;;   (interactive)
;;   (projectile-find-other-file)
;;   )

(defun +cc-find-other-file ()
  "If other file obviously in current directory, prefer that. Otherwise,
prompt."
  (interactive)
  (let ((other-naive (fn-find-other-file)))
	(if other-naive
		(find-file other-naive)
	  (projectile-find-other-file))))

;; -- indentation style -------------------
;; check C-c . for styles
;; (push '(other . "k&r") c-default-style)

;; if this doesnt work for you, set your own (offsets, inlines, bla blah)
;; (c-add-style "mystyle"
;;              '((indent-tabs-mode . nil)
;;                (c-basic-offset 5))

;; check out c-style-alist in ielm for a list of variables, ....


;; -- code completion with clang -----------
;; write a .clang_complete in the project root
;; google for ccargs, a python script to generate these files for you

;; to use it, use CXX='cc_args.py g++' before a cmake call, such that it writes
;; to the clang file as required, and then calls the compiler with this, on
;; every compilation via make, the project is compile AND the important
;; clang_complete files (which get generated automatically...)

;; then:  combine all these files to the root .clang_complete file like so
;;    find . | ag clang_complete | xargs cat | sort | uniq | >> ../../.clang.complete

;; then, re-open company-clang-arguments variable (after-reopening) [used to be
;; a bug in here where there it was set only if flycheck was installed, ...],
;; and see that company-clang-arguments has all the arguments that were in the file :)

;; syntax checking:  SPC-t-s   (with flycheck?)
;; to fix some false-alarms here, turn off warning stuff by adding
;;    -Wno-unused-parameter

;; ----------------------
;; use ycmd layer for better performance?  but its a pain to setup, ...
;;

;; -- code folding -----------------------------------------

;; TODO:  make sure to stay within the current context / hierarchy?
(defun next-c-defun ()
  (interactive)
  (end-of-defun)
  (end-of-defun)
  (c-beginning-of-defun)
  )

(defun prev-c-defun ()
  (interactive)
  (c-beginning-of-defun)
  )

;; (defun hide-c-defun ()
;;   (interactive)
;;   (end-of-defun)
;;   (previous-line)
;;   (sp-beginning-of-sexp)
;;   (hs-hide-block)
;;   (prev-c-defun)
;;   )

;; (defun show-c-defun()
;;   (interactive)
;;   (sp-beginning-of-next-sexp)
;;   (sp-beginning-of-next-sexp)
;;   (hs-show-block)
;;   )

(defun toggle-hiding-c-defun()
  (interactive)
  (forward-char)
  (beginning-of-defun)
  (search-forward-regexp "{")
  (hs-toggle-hiding)
  (beginning-of-defun)
  )

(defun toggle-hiding-c-all ()
  (interactive)
  ;; is local variable set?  no -> unhide all, set the variable
  ;; yes -> read its state.
  ;; if hidden == t, unhide all
  ;; if hidden == nil, hide all
  (save-restriction
	(c-narrow-to-most-enclosing-decl-block)
	(if (local-variable-p 'hidden)
		(if (equal hidden t)
			(progn
			  (hs-show-all)
			  (setq hidden nil))
		  (progn
			(hs-hide-all)
			(setq hidden t)))
	  (progn
		(set (make-local-variable 'hidden) nil)
		(hs-show-all)))))

;; (defun cpp-code-folding ()
;; (global-set-key (kbd "<C-tab>") 'toggle-hiding-c-defun)
;; (global-set-key (kbd "<S-iso-lefttab>") 'toggle-hiding-c-all)
;; (define-key custom-mode-map (kbd "<S-iso-lefttab>") 'widget-backward)  ;; TODO: find more elegant solution to this?
;; (global-set-key (kbd "M-n")  'next-c-defun)
;; (global-set-key (kbd "M-p")  'prev-c-defun)
;; )


;; ---------------------------------------------------------

;; c++-specific -> c++ module

(defface font-lock-method-call-face
  '((t (:foreground "orangered")))
  "Face to display method calls in."
  )

;; method calls
(font-lock-add-keywords 'c++-mode
						`((,(concat
							 "\\(?:\\.\\|->\\)"	; Member access
							 "\\s *"			; Optional white space
							 "\\<\\([_a-zA-Z][_a-zA-Z0-9]*\\)\\>" ; Member identifier
							 "\\s *"	; Optional white space
							 "(")		; Paren for method invocation
						   1 'font-lock-method-call-face
						   nil))	 ; do not override an existing face at point
						t ) 			; add this to the end of the list so everything else comes before

(font-lock-add-keywords 'c-mode
						`((,(concat
							 "\\(?:\\.\\|->\\)"	; Member access
							 "\\s *"			; Optional white space
							 "\\<\\([_a-zA-Z][_a-zA-Z0-9]*\\)\\>" ; Member identifier
							 "\\s *"	; Optional white space
							 "(")		; Paren for method invocation
						   1 'font-lock-method-call-face
						   nil))	 ; do not override an existing face at point
						t ) 			; add this to the end of the list so everything else comes before

;; ---------------------------------------------------------
(defface font-lock-function-call-face
  '((t (:foreground "cyan")))
  "Face to display non-method function calls in.")

;; function calls
(font-lock-add-keywords 'c++-mode
						`((,(concat
							 "\\<\\([_a-zA-Z][_a-zA-Z0-9]*\\)\\>" ; Member identifier
							 "\\s *"	; Optional white space
							 "(")		; Paren for method invocation
						   1 'font-lock-function-call-face
						   nil))	 	; do not override an existing face at point
						t ) 			; add this to the end of the list so everything else comes before

(font-lock-add-keywords 'c-mode
						`((,(concat
							 "\\<\\([_a-zA-Z][_a-zA-Z0-9]*\\)\\>" ; Member identifier
							 "\\s *"	; Optional white space
							 "(")		; Paren for method invocation
						   1 'font-lock-function-call-face
						   nil))	 	; do not override an existing face at point
						t ) 			; add this to the end of the list so everything else comes before



;; (add-hook 'c++-mode-hook 'cpp-code-folding)
;; (add-hook 'c-mode-hook   'cpp-code-folding)
;; (remove-hook 'c-mode-hook   'cpp-code-folding)


;;;; c++

;; -> c++ module

(defun fn--c++-template-args-cont-closing-on-0 (langelem)
  "Controls indentation of template parameters, handling the
special case of '>. Possible return values, as for all
indentation functions, is either an integer, or NIL. An integer
indicates the column to which indentation should take place. Nil
means to run the next lineup function.

This function returns either
0 : The first non-ws character is '>'. Line it up under 'template'.
nil: Otherwise, return nil and run next lineup function."
  (save-excursion
	(beginning-of-line)
	(if (re-search-forward "^[\t ]*>" (line-end-position) t)
		0)))

;; TODO: fix template arg indentation when template inside a function call (or decltype "call")

(defun fn--c++-template-args-cont-align (langelem)
  "Controls indentation of template parameters, adding
c-basic-offset to the indentation of the symbol that opens the <
brace."
  (c-lineup-template-args langelem)
  (save-excursion
	(goto-char (cdr langelem))
	(if (re-search-forward "<")
		(progn
		  (evil-backward-WORD-begin)
		  (vector (+ (current-column) c-basic-offset 1)))
	  nil))
  ;; (fn--c++-template-args-cont c-lineup-template-args +)
  )

(defun +cc-arglist-cont-nonempty-align-if-args (langelem)
  "Aligns if clause-arguments underneath each other."
  (let ((in-if-clause nil))
	(save-excursion
	  (goto-char (cdr langelem))
	  (when (re-search-forward "if\(\s*" (line-end-position) t)
		(setf in-if-clause t)))
	(if in-if-clause
	 (c-lineup-arglist langelem)
	 nil)))

;; TODO:  emacs features available for working with the C++ AST:
;;  (c-get-syntactic-indentation LANGELEM)
;;  (c-guess-basic-syntax)
;;  (cdr LANGELEM) : the point for where that statement starts

(defun fn--c++-member-init-cont (langelem)
  "Controls indentation of subsequent member initialization list
lines. Aligns elements on the lading comma or semicolon."
  (save-excursion
	(goto-char (cdr langelem))
	(if (re-search-backward ":\\|,")
		(progn
		  (vector (current-column)))
	  nil)))

;; TODO: detect passing a braced-list to a constructor on a new line, and put
;; the braced-list indentation relative to the constructor-symbol

;; (defun c-lineup-template-args (_langelem)
;;   "Line up template argument lines under the first argument.
;; To allow this function to be used in a list expression, nil is
;; returned if there's no template argument on the first line.

;; Works with: template-args-cont."
;;   (save-excursion
;;     (c-with-syntax-table c++-template-syntax-table
;;       (beginning-of-line)
;;       (backward-up-list 1)
;;       (if (and (eq (char-after) ?<)
;; 	       (zerop (c-forward-token-2 1 nil (c-point 'eol))))
;; 	  (vector (current-column))
;; 	10
;; 	)
;;       )))

;; (remove-hook 'c++-mode-hook
;; 		  (lambda ()
;; 			(c-set-offset 'template-args-cont
;; 						  '(fn--c++-template-args-cont-closing-on-0 c-lineup-template-args +))))

;;;;; keys
(define-key c++-mode-map (kbd "<f4>") '+cc-find-other-file)
;; (define-key c++-mode-map (kbd "C-e") 'helm-gtags-find-tag)
;; (define-key c++-mode-map (kbd "C-S-e") 'helm-gtags-find-tag-other-window)
;; (define-key c++-mode-map (kbd "<M-left>") 'helm-gtags-previous-history)
;; (define-key c++-mode-map (kbd "<M-right>") 'helm-gtags-next-history)
;; (define-key c++-mode-map (kbd "C-S-f") 'helm-gtags-find-pattern)
(define-key c++-mode-map (kbd "C-SPC") 'company-complete)
(define-key c++-mode-map (kbd "C-@") 'company-complete)
;; (define-key c++-mode-map (kbd "<C-tab>") #'outline-cycle)

(define-key c-mode-map (kbd "<f4>") '+cc-find-other-file)
;; (define-key c-mode-map (kbd "C-e") 'helm-gtags-find-tag)
;; (define-key c-mode-map (kbd "C-S-e") 'helm-gtags-find-tag-other-window)
;; (define-key c-mode-map (kbd "<M-left>") 'helm-gtags-previous-history)
;; (define-key c-mode-map (kbd "<M-right>") 'helm-gtags-next-history)
;; (define-key c-mode-map (kbd "C-S-f") 'helm-gtags-find-pattern)
(define-key c-mode-map (kbd "C-SPC") 'company-complete)
(define-key c-mode-map (kbd "C-@") 'company-complete)

(require 'lsp-mode)
(define-key c++-mode-map (kbd "C-/") #'+lsp-trigger-ui-doc)
(define-key c++-mode-map (kbd "C-_") #'+lsp-trigger-ui-doc)
(define-key c-mode-map (kbd "C-/") #'+lsp-trigger-ui-doc)
(define-key c-mode-map (kbd "C-_") #'+lsp-trigger-ui-doc)

(defun cpp-mode-config ()
  (spacemacs/toggle-truncate-lines-on)
  (yascroll-bar-mode -1)
  (setq indent-tabs-mode t)
  ;; (infer-indentation-style)
  (c-toggle-auto-newline -1)
  (setq c-auto-newline nil)
  ;; offsets
  (c-set-offset 'innamespace 0)
  (rainbow-delimiters-mode -1)
  ;; (c-set-offset ')
  ;; (c-set-offset 'template-args-cont
  ;; 				'(fn--c++-template-args-cont-closing-on-0 c-lineup-template-args +))
  (electric-indent-mode -1)
  ;; -------------
  (modern-c++-font-lock-mode 1)
  (filladapt-mode 1)
  )

(add-hook 'c++-mode-hook 'cpp-mode-config)

(defun c-mode-config ()
  (spacemacs/toggle-truncate-lines-on)
  (yascroll-bar-mode 1)
  (setq indent-tabs-mode t)
  ;; (infer-indentation-style)
  (c-toggle-auto-newline -1)
  (setq c-auto-newline nil)
  ;; offsets
  (c-set-offset 'innamespace 0)
  (rainbow-delimiters-mode -1)
  ;; (c-set-offset ')
  (c-set-offset 'template-args-cont
				'(fn--c++-template-args-cont-closing-on-0 c-lineup-template-args +))
  (paren-face-mode 1)
  (electric-indent-mode -1)
  ;; -------------
  (setq tab-width 8)
  (modern-c++-font-lock-mode 1)
  (filladapt-mode 1)
  )

(add-hook 'c-mode-hook   'c-mode-config)

;;;;; doxygen formatting

;; -> c++

(defface doxygen-verbatim-face
  '((default :inherit default))
  "Face used to show Doxygen block regions"
  :group 'font-lock-faces)

(defface doxygen-match-face
  '((default :inherit default)
	(t :underline t))
  "Face used to show Doxygen region start end commands"
  :group 'font-lock-faces)

(defconst custom-font-lock-doc-comments
  `(
	;; Highlight Doxygen special commands,
	;;   \cmd or @cmd
	;; and the non [a-z]+ commands
	;;   \\ \@ \& \# \< \> \% \" \. \| \-- \--- \~[LanguageId]
	(,(concat
	   "\\(?:"
	   "[\\@][a-z]+" ;; typical word Doxygen special @cmd or \cmd
	   "\\|"
	   ;; non-word commands, e.g. \\ or @\
	   "[\\@]\\(?:\\\\\\|@\\|&\\|#\\|<\\|>\\|%\\|\"\\|\\.\\|::\\||\\|---?\\|~[a-z]*\\)"
	   "\\)")
	 0 ,c-doc-markup-face-name prepend nil)
	;; Highlight autolinks. These are referring to functions, so we use a different font face
	;; from the Doxygen special commands.
	(,(concat
	   "\\(?:"
	   ;; function() or function(int, std::string&, void*) or more complex where we only
	   ;; match the first paren, function(x->(), 2*(y+z)).
	   "[A-Za-z_0-9]+(\\([A-Za-z_0-9:&*, ]*)\\)?"
	   ;; ClassName::memberFcn or the destructor ClassName::~ClassName. Can also do unqualified
	   ;; references, e.g. ::member. The parens are optional, ::member(int, int), ::member(a, b).
	   ;; We only require matching of first paren to make cases like ::member(x->(), 2*(y+z))
	   ;; work. We don't want \::thing to be highlighed as a function, hence reason to look for
	   ;; class::member or space before ::member.  Note '#' can be used instead of '::'
	   "\\|"
	   "\\(?:[A-Za-z_0-9]+\\|\\s-\\)\\(?:::\\|#\\)~?[A-Za-z_0-9]+(?\\(?:[A-Za-z_0-9:&*, \t]*)\\)?"
	   ;; file.cpp, foo/file.cpp, etc. Don't want to pickup "e.g." or foo.txt because
	   ;; these are not autolinked so look for common C++ extensions.
	   "\\|"
	   "[A-Za-z_0-9/]+\\.\\(?:cpp\\|cxx\\|cc\\|c\\|hpp\\|hxx\\|hh\\|h\\)"
	   "\\)")
	 0 font-lock-function-name-face prepend nil)
	;; Highlight URLs, e.g. http://doxygen.nl/autolink.html note we do this
	;; after autolinks highlighting (we don't want nl/autolink.h to be file color).
	("https?://[^[:space:][:cntrl:]]+"
	 0 font-lock-keyword-face prepend nil)
	;; Highlight HTML tags - these are processed by Doxygen, e.g. <b> ... </b>
	(,(concat "</?\\sw"
			  "\\("
			  (concat "\\sw\\|\\s \\|[=\n\r*.:]\\|"
					  "\"[^\"]*\"\\|'[^']*'")
			  "\\)*>")
	 0 ,c-doc-markup-face-name prepend nil)
	;; E-mails, e.g. first.last@domain.com. We don't want @domain to be picked up as a Doxygen
	;; special command, thus explicitly look for e-mails and given them a different face than the
	;; Doxygen special commands.
	("[A-Za-z0-9.]+@[A-Za-z0-9_]+\\.[A-Za-z0-9_.]+"
	 0 font-lock-keyword-face prepend nil)
	;; Quotes: Doxygen special commands, etc. can't be in strings when on same line, e.g.
	;; "foo @b bar line2 @todo foobar" will not bold or create todo's.
	("\"[^\"[:cntrl:]]+\""
	 0 ,c-doc-face-name prepend nil)

	("[^\\@]\\([\\@]f.+?[\\@]f\\$\\)" ;; single line formula but an escaped formula, e.g. \\f[
	 1 'doxygen-verbatim-face prepend nil)

	;; Doxygen verbatim/code/formula blocks should be shown using doxygen-verbatim-face, but
	;; we can't do that easily, so for now flag the block start/ends
	(,(concat
	   "[^\\@]"	;; @@code shouldn't be matched
	   "\\([\\@]\\(?:verbatim\\|endverbatim\\|code\\|endcode\\|f{\\|f\\[\\|f}\\|f]\\)\\)")
	 1 'doxygen-match-face prepend nil)

	;; Here's an attempt to get blocks shown using doxygen-verbatim-face. However, font-lock doesn't
	;; support multi-line font-locking by default and I'm not sure the best way to make these work.
	;;
	;; Doxygen special commands, etc. can't be in verbatim/code blocks
	;;   @verbatim
	;;      @cmd  -> not a Doxygen special command
	;;   @endverbatim
	;; so set verbatim/code to a different font.  Verbatim/code blocks spans multiple lines and thus
	;; a refresh of a buffer after editing a verbatim/code block may be required to have the font
	;; updated.
	;;("[^\\@][\\@]\\(verbatim\\|code\\)\\([[:ascii:][:nonascii:]]+?\\)[\\@]end\\1"
	;; 2 'doxygen-verbatim-face prepend nil)
	;; Doxygen formulas are link verbatim blocks, but contain LaTeX, e.g.
	;;("[^\\@][\\@]f.+[\\@f]\\$"  ;; single line formula
	;; 0 'doxygen-verbatim-face prepend nil)
	;; multi-line formula,
	;;   \f[ ... \f]     or    \f{ ... \}
	;;("[^\\@][\\@]f\\(?:{\\|\\[\\)\\([[:ascii:][:nonascii:]]+?\\)[\\@]f\\(?:}\\|\\]\\)"
	;; 1 'doxygen-verbatim-face prepend nil)

	))

;; Matches across multiple lines:
;;   /** doxy comments */
;;   /*! doxy comments */
;;   /// doxy comments
;; Doesn't match:
;;   /*******/
(defconst custom-font-lock-keywords
  `((,(lambda (limit)
		(c-font-lock-doc-comments "/\\(//\\|\\*[\\*!][^\\*!]\\)"
			limit custom-font-lock-doc-comments)))))

(setq-default c-doc-comment-style (quote (custom)))


(require 'modern-cpp-font-lock)
(modern-c++-font-lock-global-mode)

;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;; (setf auto-mode-alist (remove '("\\.h\\'" . c++-mode) auto-mode-alist))

;; (c-or-c++-mode) on .h files
;; adjust this if it doesn't work:
;;
;; (setq c-or-c++-mode--regexp "^[		
;; ]*\\(?:using[	
;; ]+\\(?:namespace[	
;; ]+std;\\|std::\\)\\|namespace\\(:?[		
;; ]+[a-zA-Z0-9_]+\\)?[	
;; ]*{\\|class[	
;; ]+[a-zA-Z0-9_]+[	
;; ]*[:{
;; ;; ]\\|template[	
;; ]*<.*>\\|#include[	
;; ]*<\\(?:string\\|iostream\\|map\\)>\\)")

;;;;; navigation /folding

;; semantically:  C-
(evil-define-key '(normal visual) c++-mode-map
  (kbd "C-n") '(lambda () (interactive)  (beginning-of-defun -1))
  (kbd "C-p") '(lambda () (interactive) (beginning-of-defun 1))
  (kbd "<C-tab>") nil
  (kbd "<C-M-tab>") #'toggle-hiding-c-all
  )
(define-key c++-mode-map (kbd "C-<tab>") #'toggle-hiding-c-defun)
(define-key c++-mode-map (kbd "<C-M-tab>") #'toggle-hiding-c-all)

(evil-define-key '(normal visual) c-mode-map
  (kbd "C-n") '(lambda () (interactive)  (beginning-of-defun -1))
  (kbd "C-p") '(lambda () (interactive) (beginning-of-defun 1))
  (kbd "<C-tab>") nil
  (kbd "<C-M-tab>") #'toggle-hiding-c-all
  )
(define-key c-mode-map (kbd "C-<tab>") #'toggle-hiding-c-defun)
(define-key c-mode-map (kbd "<C-M-tab>") #'toggle-hiding-c-all)

;; outlines:  M-
(evil-define-key '(normal visual insert) c++-mode-map
  (kbd "M-n") 'outline-next-heading
  (kbd "M-p") 'outline-previous-heading
  (kbd "<backtab>") #'outshine-cycle-buffer
  )

(defun +cc-open-line-below ()
  (interactive)
  (if (nth 4 (syntax-ppss))
	  ;; inside comment
	  ;;  -> insert new line, add comment thing, go back up
	  ()
	;; outside comment
	;; -> default evil behaviour
	)
  )

(defun +cc-open-line-above ()
  (interactive)
  (if (nth 4 (syntax-ppss))
	  ;; inside comment
	  ;;  -> goto beginning of COMMENT line, call (c-context-open-line)
	  ()
	;; outside comment
	;; -> default evil behaviour
	)
  )


(defun +cc-in-single-line-comment-p ()
  (and (nth 4 (syntax-ppss))
	   (save-excursion
		 (beginning-of-line)
		 (when
			 (re-search-forward "^[\t\\|\s]*\\/\\/" (line-end-position) t)
		   t))))

(defun +cc-in-multi-line-comment-p ()
  (and (nth 4 (syntax-ppss))
	   (not (+cc-in-single-line-comment-p))))

(defun +cc-single-line-comment-to-multi ()
  ;; precondition:   in a single-line comment
  ;; - extract string
  ;; - insert bla, with string

  (let* ((comment (save-excursion
					(beginning-of-line)
					(re-search-forward "^[\t\\|\s]*//\\(...*\\)" (line-end-position) t)
					(match-string 1)))
		 (comment-start (ignore-errors (- (match-beginning 1) 2)))
		 (first-column nil)
		 (target nil)
		 )
	(when (and comment comment-start)
	  (goto-char comment-start)
	  (setf first-column (current-column))
	  (kill-line)
	  (insert "/*")
	  (c-context-line-break)
	  (insert comment)
	  (c-context-line-break)
	  (setf target (point))
	  (newline)
	  (move-to-column (+ 1 first-column) t)
	  (insert "*/")
	  (goto-char target)
	  (evil-insert 0))))

(defun +cc-M-ret-handler ()
  "Perform DWIM action at point."
  (interactive)
  (cond ((+cc-in-single-line-comment-p)
		 ;; transform to multi-line comment
		 (+cc-single-line-comment-to-multi)
		 )
		((+cc-in-multi-line-comment-p)
		 ;; do something special, for now:  just newline
		 (c-context-line-break)
		 )
		(t (outshine-insert-heading))))

(evil-define-key '(normal visual insert) c++-mode-map (kbd "M-RET") #'+cc-M-ret-handler)
(evil-define-key '(normal visual insert) c-mode-map (kbd "M-RET") #'+cc-M-ret-handler)

(evil-define-key '(insert) c++-mode-map
  (kbd "<RET>") #'c-context-line-break
  )

(evil-define-key '(normal) c++-mode-map
  (kbd "o") #'c-context-line-break
  )

(evil-define-key '(normal visual insert) c-mode-map
  (kbd "M-n") 'outline-next-heading
  (kbd "M-p") 'outline-previous-heading
  (kbd "<backtab>") #'outshine-cycle-buffer
  (kbd "<RET>") #'c-context-line-break
  )

(evil-define-key '(insert) c-mode-map
  (kbd "<RET>") #'c-context-line-break
  )

;;;;; comments

;; fill-prefix:  nil for filladapt-mode requirement

;; (defun +cc-indent-line-function ()
;;   "Wrapper around c-indent-line-function to  "

;;   )

;;;;; file/class creation

(defun +cc--prop-string (s)
  (propertize s 'face '(:foreground "#ac443f")))

(defun +cc--get-path ()
  "Queries for a path from default-directory in the form
  <default-directory>/NAME, creating directories as necessary,
  checking the result for validity, erroring out if necessary,
  returning string tuple (PATH,CLASSNAME) on success."
  (let* ((full-path (read-string (+cc--prop-string "Path: ")
								 (concat (f-slash default-directory) "classname")))
		 (dir (f-dirname full-path))
		 (classname (f-filename full-path)))
	(when (or (string= "" dir) (string= "" classname))
	  (error "Invalid path or classname"))
	(list (f-slash dir) classname)))

(defvar +cc--default-namespaces (list "indurad" "iloadout")
  "A list of namespaces new classes and methods created with
  +cc-new-class are put below by-default.")

(defun +cc--query-list (query &optional default)
  "Queries for a list of namespaces, defaulting to +cc--default-namespaces."
  (mapcar #'symbol-name (read (read-string (+cc--prop-string query)
										   (if default (format "%s" default)
											 "()")))))

(defun +cc--query-val (query &optional default)
  (read-string (+cc--prop-string query)
			   (when default (format "%s" default))))

(defun random-alnum ()
  (let* ((alnum "abcdefghijklmnopqrstuvwxyz0123456789")
         (i (% (abs (random)) (length alnum))))
    (substring alnum i (1+ i)))
  )

(defun +cc--gen-random-hash ()
  (labels ((random-alnum ()
			(let* ((alnum "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
				   (i (% (abs (random)) (length alnum))))
			  (substring alnum i (1+ i)))))
	(mapconcat (lambda (s) (random-alnum)) '("" "" "" "" "") "")))

(defun +cc--expand-snippet (name env)
  "Expands snippet NAME at point. "
  (insert name)
  (yas-expand-snippet (yas-lookup-snippet name) (point) (point) env))

(defun +cc-new-class ()
  "Creates file for class given name in default-directory, put in
  namespaces as queried, creating custom include-guards,
  inheriting from classes as queried and creating sane defaults
  for known ones (i.e. iframework operators etc.)
   pre-creating a constructor, and methods."
  (interactive)
  (-let* ((yas-snippet-dirs (list (concat dir_emacs "/share/newfile/new")))
		  ((directory classname) (+cc--get-path) )
		  (header (concat (f-slash directory) classname ".h"))
		  (source (concat (f-slash directory) classname ".cpp")))

	;; existing files?
	(when (or (f-exists? header) (f-exists? source))
	  (when (not (y-or-n-p "File(s) exist, overwrite?"))
		(error "Cancelled.")))

	(-let* ((namespaces (+cc--query-list "Namespaces: " +cc--default-namespaces))
			(baseclass (+cc--query-val "Base-Class: " ))
			(include-guard (concat (s-upcase classname) "_H_" (+cc--gen-random-hash))))

	  ;; header-file
	  (with-temp-buffer
		(c++-mode)
		(yas-expand-snippet (yas-lookup-snippet "header")
							(point)
							(point)
							(list `(classname ,classname)
								  `(include-guard ,include-guard)
								  `(namespaces (list ,@namespaces))
								  `(baseclass ,baseclass)))
		(write-file header))

	  ;; source-file
	  (with-temp-buffer
		(c++-mode)
		(yas-expand-snippet (yas-lookup-snippet "source")
							(point)
							(point)
							(list `(classname ,classname)
								  `(namespaces (list ,@namespaces))
								  `(baseclass ,baseclass)))
		(write-file source))

	  ;; close existing buffers of that name and path
	  (let ((header-buf (get-buffer (concat classname ".h")))
			(source-buf (get-buffer (concat classname ".cpp"))))
		(when (string= (buffer-file-name header-buf) header)
		  (kill-buffer header-buf))
		(when (string= (buffer-file-name source-buf) source)
		  (kill-buffer source-buf)))

	  ;; show created header
	  (let ((buffer (find-file-noselect header t)))
		(if (listp buffer)
			(mapcar 'pop-to-buffer-same-window (nreverse buffer))
		  (pop-to-buffer-same-window buffer)))

	  ;; stage new files, ignoring if there are errors
	  (ignore-errors
		(magit-stage-file source)
		(magit-stage-file header))

	  ;; optionally show nearest cmakelists
	  (when (y-or-n-p "Find nearest CMakeLists.txt?")
		(let ((cmakelists-dir (locate-dominating-file default-directory "CMakeLists.txt")))
		  (when cmakelists-dir
			(find-file (concat (f-slash cmakelists-dir) "CMakeLists.txt" ) )))))))





