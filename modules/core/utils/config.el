;;; modules/core/utils/config.el       -*- lexical-binding: t -*-


;;; module management

;;;; creation
(defun +utils-module-create (subdir)
  "Generate new module in dir_emacs/modules/SUBDIR."

  (let* ((subdir (s-chop-suffix "/" (s-chop-prefix "/" subdir)))
		 (path (concat dir_emacs
					   "modules/"
					   subdir)))

	;; dir
	(when (not (f-directory? path))
	  (make-directory path t))

	;; config.el
	(let ((path-config-el (concat path "/config.el")))
	  (when (not (f-file? path-config-el))
		(f-touch path-config-el)

		;; header
		(with-temp-buffer
		  (insert (concat  ";; modules/" subdir "/config.el    -*- lexical-binding: t; -*-"))
		  (newline 5)
		  (append-to-file (point-min) (point-max) path-config-el ))))))

;; (setq modules '(
;; 				"core/on-exit"
;; 				"core/on-startup"
;; 				"core/encryption"
;; 				"theme"
;; 				"ui/general"
;; 				"ui/window-navigation"
;; 				"ui/code-navigation"
;; 				"feature/general"
;; 				"feature/magit"
;; 				"feature/git-gutter"
;; 				"feature/printing"
;; 				"editor/general"
;; 				"editor/scrolling"
;; 				"editor/code-folding"
;; 				"editor/horizontal-rulers"
;; 				"editor/multiple-cursors"
;; 				"editor/code-folding"
;; 				"lang/java"
;; 				"lang/cc"))
;; (cl-loop for m in modules do
;; 		 (+utils-module-create m))

(setq +utils--last-module-path "")

(defun +utils-move-region-to-module (&optional module-path)
  "Moves the region to config.el of module in MODULE-PATH."
  (interactive)
  (let* ((module-path (if (null module-path) (read-from-minibuffer "> " +utils--last-module-path)
						module-path))
		 (path (concat dir_emacs
					   "modules/"
					   (s-chop-suffix "/" (s-chop-prefix "/" module-path)))))
	(append-to-file (region-beginning) (region-end) (concat path "/config.el"))
	(setq +utils--last-module-path module-path)
	(delete-region (region-beginning) (region-end))))

(spacemacs/set-leader-keys "y" #'+utils-move-region-to-module)

;;;; loading .el files without setting load path
(defun +load (fpath)
  (let ((current-filename (or load-file-name
							  (buffer-file-name))))
	(if (stringp current-filename)
		(load-file
		 (expand-file-name fpath
						   (file-name-directory current-filename)))
	  (message (concat "tried to load " fpath ", but it failed."))
	  )))

;;;; reporting

(defun +utils-modules-list (&optional insert-here)
  "List modules (<-> directories with config.el). If INSERT-HERE
is t, insert the output in the current buffer"
  (interactive)

  ;; for every directory that has a config.el in it,
  ;; print the path relative to the module dir

  (let* ((cmd "fd config.el")
		 (default-directory (concat dir_emacs "modules"))
		 (cands
		  (mapcar #'(lambda (s) (s-chop-suffix "/config.el" s))
				  (s-split "\n" (with-output-to-string
								  (with-current-buffer standard-output
									(shell-command cmd t)))))))
	(cl-loop for s in cands do
			 (if insert-here
				 (insert (concat s "\n"))
			   (message s)))
	)
  )


;;;; ui: file <-> config.el toggle

(defvar +utils-modules--last-toggle '(nil . nil))

(defun +utils-modules-toggle-config ()
  (interactive)
  (let* ((module (f-dirname (buffer-file-name)))
		 (filename (f-filename (buffer-file-name))))
	(if (string= filename "config.el")
		(if (string= (car +utils-modules--last-toggle) module)
			(find-file (concat default-directory (cdr +utils-modules--last-toggle))))
	  (progn
		(setf +utils-modules--last-toggle `(,module . ,filename))
		(find-file (concat default-directory "config.el"))))))


;;;; current directory

(defun +utils-pwd ()
  "Return current path, without 'Directory ' prefix as (pwd)
does."
  (interactive)
  default-directory)

(defun +utils-pwd-to-clipboard ()
  (interactive)
  (cut-function (+utils-pwd))
  )
(defalias 'c. '+utils-pwd-to-clipboard)


;;;; authinfo

(defun +utils-authinfo-init-p ()
  (ignore-errors
	(if (auth-source-search :max 1 :host "irc.freenode.net") t nil))
  )


;;;; side-windows
(require 'dash)

(defun +utils-side-window-p (&optional window frame)
  (let ((window (if window window (selected-window)))
		(frame (if frame frame (selected-frame)))
		)
	(if (member window
				(-filter (lambda (win) (window-parameter win 'window-side))
						 (window-list)))
		t nil)))


;; (defun fn-add-or-modify-alist (key val alist)
;;   "Add (key . val) to alist if no key-entry existing. Change
;;   value if it does. "
;;   (if (null (alist-get key alist))
;; 	  (setq alist (push (cons key val) alist))
;; 	(setf (push alist key) val))
;;   alist)

;; (defun fn-get-or-add-alist (key alist)
;;   "Get key from alist or add (key . nil) if it does not exist."
;;   (let (( content (assoc key alist) ))
;; 	(if (null content)
;; 		(push (key . nil) alist)
;; 	  ))
;;   )

;; (defun fn-alist-create-if-non-existing ( key alist )
;;   (if (null (assoc key alist))
;; 	  (setq alist (push (cons key nil) alist))
;; 	alist))

;; -- anaphoric macros -----------------

(defmacro aif (test then &optional else)
  `(let ((it ,test))
	 (if it
		 ,then
	   ,else)))

;; (aif (car (member 5 '(1 2 3 5))) (+ 1 it) 10 )
;; (if-let ((x 5) (y 10)) y (message "else"))           ;; emacs 25 macros
;; (when-let ((x (progn (message "1") nil)) (y (progn (message "2") 10))) (message "here"))

(defmacro awhen (test &body body)
  `(aif ,test
		(progn ,@body)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
	   ((not it))
	 ,@body))

(defmacro aand (&rest args)
  (cond ((null args) t)
		((null (cdr args)) (car args))
		(t `(aif ,(car args) (aand ,@(cdr args))))))

;; ...continue evaluation iff every form true
;; ...use it in forms to use last result
;; (aand 5 (+ it 10) (< it 20) (message "here!"))

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
          (sym (gensym)))
      `(let ((,sym ,(car cl1)))
         (if ,sym
             (let ((it ,sym)) ,@(cdr cl1))
           (acond ,@(cdr clauses)))))))

;; (acond ((< 10 5) 5) ((+ 10 5) it ))

;; let-alist equivalents for plists/property-lists: 
(defmacro with-dict (key-vals &rest body)
  "A context-manager for a plist where each key is a callable
function that returns the value."
  (declare (indent 1))
  (let* ((g (if (symbolp key-vals)
                (symbol-value key-vals)
              key-vals))
         (keys (-slice g 0 nil 2)))
    `(labels ,(loop for key in keys
                    collect
                    (list key '() `(plist-get ',g ,key)))
       ,@body)))

;; (with-dict (:a 1 :b 'some-symbol :c 3)
;;            (:b))

(defmacro with-plist-vals (plist &rest body)
  "Bind the values of a plist to variables with the name of the keys."
  (declare (indent 1))
  `(let ,(loop for key in (-slice plist 0 nil 2)
               for val in (-slice plist 1 nil 2)
               collect (list (intern
                              (substring (symbol-name key) 1))
                             val))
     ,@body))

;; Used like:

;; (with-plist-vals (:a 4 :b 6)
;;                  (* 2 a))

;;;; setf'able association list navigation

(defun +alist-get (keys alist)
  "Recursively traverses ALIST to return the position pointed to
  by KEYS." (if (or (null keys)
		  (null alist))
	nil
	(cl-labels ((lin-search
				 (keys_ alist_)
				 (cond
				  ((null alist_) nil)					 ; not found, return nil
				  ((null keys_) (cdr alist_))			 ; found, return value
				  (t (lin-search (cdr keys_)
								 (assoc (car keys_) alist_))))))
	  (lin-search keys alist))))

(defun +alist-set (keys alist val)
  "Recursively traverses ALIST to setf the position pointed to by
  KEYS to VAL"
  (if (or (null keys)
		  (null alist))
	  nil
	(cl-labels ((lin-search
				 (keys_ alist_ val_)
				 (cond
				  ((null alist_) nil)	; not found, return nil
				  ((null keys_) (setf (cdr alist_) val_)) ; found, return value
				  (t (lin-search (cdr keys_)
								 (assoc (car keys_) alist_)
								 val_)))))
	  (lin-search keys alist val))))

(gv-define-simple-setter +alist-get +alist-set)


;;;; error handling

;; This gist licensed GPLv3
(cl-defmacro condition-case+ (var form (&rest handlers/else/finally))
  "Like condition-case, only if the last handlers have matching
forms of :else or :finally. In that case, the body of an :else
handler is evaluated if no exception was thrown. The body of
a :finally clause is evaluated always as the last thing before
the form is exited whether normally or not. If both :else
and :finally appear among the handlers, :else must be second last
and :finally must be last."
  (cl-flet ((maybe-split-last (symbol handlers)
							  (let ((last-handler (last handlers)))
								(cond
								 ((and (listp (car last-handler))
									   (eq (caar last-handler) symbol))
								  (list (butlast handlers)
										(cdar last-handler)))
								 (t
								  (list handlers nil))))))
    (destructuring-bind (handlers finally) (maybe-split-last :finally handlers/else/finally)
      (destructuring-bind (handlers else) (maybe-split-last :else handlers)
        (let ((errant-matchers (cl-loop for (match . handler-body) in handlers
                                        if (member match '(:else :finally))
                                        collect match)))
          (when errant-matchers
            (error "Matcher(s) %s found out of place. Please read the documentation"
                   (mapconcat 'prin1-to-string errant-matchers ", "))))

        (let* ((success? (cl-gensym "success?"))
               (body `(let (,success?)
                        (condition-case ,var
                            (prog1
                                ,form
                              (setq ,success? t))
                          ,@handlers)
                        ,@(if else
                              `((when ,success?
                                  ,@(if (listp else)
                                        else
                                      (list else))))))))
          (if finally
              `(unwind-protect
                   ,body
                 ,@finally)
            body))))))

;;;; benchmarking

(cl-defmacro bench (&optional (times 100000) &rest body)
  "Call `benchmark-run-compiled' on BODY with TIMES iterations, returning list suitable for Org source block evaluation.
Garbage is collected before calling `benchmark-run-compiled' to
avoid counting existing garbage which needs collection."
  (declare (indent defun))
  `(progn
     (garbage-collect)
     (list '("Total runtime" "# of GCs" "Total GC runtime")
           'hline
           (benchmark-run-compiled ,times
             (progn
               ,@body)))))

(cl-defmacro bench-multi (&key (times 1) forms ensure-equal raw)
  "Return Org table as a list with benchmark results for FORMS.
 Runs FORMS with `benchmark-run-compiled' for TIMES iterations.

 When ENSURE-EQUAL is non-nil, the results of FORMS are compared,
 and an error is raised if they aren't `equal'. If the results
 are sequences, the difference between them is shown with
 `seq-difference'.

 When RAW is non-nil, the raw results from
 `benchmark-run-compiled' are returned instead of an Org table
 list.

 If the first element of a form is a string, it's used as the
 form's description in the bench-multi-results; otherwise, forms
 are numbered from 0.

 Before each form is run, `garbage-collect' is called."
  ;; MAYBE: Since `bench-multi-lexical' byte-compiles the file, I'm not sure if
  ;; `benchmark-run-compiled' is necessary over `benchmark-run', or if it matters.
  (declare (indent defun))
  (let*((keys (gensym "keys"))
        (result-times (gensym "result-times"))
        (header '(("Form" "x faster than next" "Total runtime" "# of GCs" "Total GC runtime")
                  hline))
        ;; Copy forms so that a subsequent call of the macro will get the original forms.
        (forms (copy-list forms))
        (descriptions (cl-loop for form in forms
                               for i from 0
                               collect (if (stringp (car form))
                                           (prog1 (car form)
                                             (setf (nth i forms) (cadr (nth i forms))))
                                         i))))
    `(unwind-protect
         (progn
           (defvar bench-multi-results nil)
           (let* ((bench-multi-results (make-hash-table))
                  (,result-times (sort (list ,@(cl-loop for form in forms
                                                        for i from 0
                                                        for description = (nth i descriptions)
                                                        collect `(progn
                                                                   (garbage-collect)
                                                                   (cons ,description
                                                                         (benchmark-run-compiled ,times
                                                                           ,(if ensure-equal
                                                                                `(puthash ,description ,form bench-multi-results)
                                                                              form))))))
                                       (lambda (a b)
                                         (< (second a) (second b))))))
             ,(when ensure-equal
                `(cl-loop with ,keys = (hash-table-keys bench-multi-results)
                          for i from 0 to (- (length ,keys) 2)
                          unless (equal (gethash (nth i ,keys) bench-multi-results)
                                        (gethash (nth (1+ i) ,keys) bench-multi-results))
                          do (if (sequencep (gethash (car (hash-table-keys bench-multi-results)) bench-multi-results))
                                 (let* ((k1) (k2)
                                        ;; If the difference in one order is nil, try in other order.
                                        (difference (or (setq k1 (nth i ,keys)
                                                              k2 (nth (1+ i) ,keys)
                                                              difference (seq-difference (gethash k1 bench-multi-results)
                                                                                         (gethash k2 bench-multi-results)))
                                                        (setq k1 (nth (1+ i) ,keys)
                                                              k2 (nth i ,keys)
                                                              difference (seq-difference (gethash k1 bench-multi-results)
                                                                                         (gethash k2 bench-multi-results))))))
                                   (user-error "Forms' bench-multi-results not equal: difference (%s - %s): %S"
                                               k1 k2 difference))
                               ;; Not a sequence
                               (user-error "Forms' bench-multi-results not equal: %s:%S %s:%S"
                                           (nth i ,keys) (nth (1+ i) ,keys)
                                           (gethash (nth i ,keys) bench-multi-results)
                                           (gethash (nth (1+ i) ,keys) bench-multi-results)))))
             ;; Add factors to times and return table
             (if ,raw
                 ,result-times
               (append ',header
                       (bench-multi-process-results ,result-times)))))
       (unintern 'bench-multi-results nil))))

(cl-defmacro bench-multi-lexical (&key (times 1) forms ensure-equal raw)
  "Return Org table as a list with benchmark results for FORMS.
Runs FORMS from a byte-compiled temp file with `lexical-binding'
enabled, using `bench-multi', which see.

Afterward, the temp file is deleted and the function used to run
the benchmark is uninterned."
  (declare (indent defun))
  `(let* ((temp-file (concat (make-temp-file "bench-multi-lexical-") ".el"))
          (fn (gensym "bench-multi-lexical-run-")))
     (with-temp-file temp-file
       (insert ";; -*- lexical-binding: t; -*-" "\n\n"
               "(defvar bench-multi-results)" "\n\n"
               (format "(defun %s () (bench-multi :times %d :ensure-equal %s :raw %s :forms %S))"
                       fn ,times ,ensure-equal ,raw ',forms)))
     (unwind-protect
         (if (byte-compile-file temp-file 'load)
             (funcall (intern (symbol-name fn)))
           (user-error "Error byte-compiling and loading temp file"))
       (delete-file temp-file)
       (unintern (symbol-name fn) nil))))

(cl-defmacro bench-dynamic-vs-lexical-binding (&key (times 1) forms ensure-equal)
  "Benchmark FORMS with both dynamic and lexical binding.
Calls `bench-multi' and `bench-multi-lexical', which see."
  (declare (indent defun))
  `(let ((dynamic (bench-multi :times ,times :ensure-equal ,ensure-equal :raw t
                    :forms ,forms))
         (lexical (bench-multi-lexical :times ,times :ensure-equal ,ensure-equal :raw t
                    :forms ,forms))
         (header '("Form" "x faster than next" "Total runtime" "# of GCs" "Total GC runtime")))
     (cl-loop for result in-ref dynamic
              do (setf (car result) (format "Dynamic: %s" (car result))))
     (cl-loop for result in-ref lexical
              do (setf (car result) (format "Lexical: %s" (car result))))
     (append (list header)
             (list 'hline)
             (bench-multi-process-results (append dynamic lexical)))))

(defun bench-multi-process-results (results)
  "Return sorted RESULTS with factors added."
  (setq results (sort results (-on #'< #'second)))
  (cl-loop with length = (length results)
           for i from 0 to (1- length)
           for description = (car (nth i results))
           for factor = (if (< i (1- length))
                            (format "%.2f" (/ (second (nth (1+ i) results))
                                              (second (nth i results))))
                          "slowest")
           collect (append (list description factor)
                           (list (format "%.6f" (second (nth i results)))
                                 (third (nth i results))
                                 (if (> (fourth (nth i results)) 0)
                                     (format "%.6f" (fourth (nth i results)))
                                   0)))))

(defun +utils-quick-peek ()
  ;; show
  ;; install handler to close on first command issued
  )

;; overwrite insert-spacer
(use-package quick-peek
  :load-path "packages/quick-peek/")
(defun quick-peek--insert-spacer (pos str-before str-after)
  "Insert a thin horizontal line at POS.
Line is surrounded by STR-BEFORE and STR-AFTER."
  nil)


