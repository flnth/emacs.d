
(defun my/py-statement-opens-base (regexp)
  (let ((orig (point))
        erg)
    (save-excursion
      (back-to-indentation)
      (python-nav-forward-statement)
      (python-nav-backward-statement)
      (when (and
             (<= (line-beginning-position) orig)(looking-back "^[ \t]*" (line-beginning-position))(looking-at regexp))
        (setq erg (point))))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defconst my/py-class-re "[ \t]*\\_<\\(class\\)\\_>[ \n\t]"
"Matches the beginning of a class definition. ")

(defun my/py-statement-opens-class-p ()
"Return `t' if the statement opens a functions or class definition, nil otherwise. "
    (my/py-statement-opens-base my/py-class-re))

(defun my/python-narrow-to-class (&optional move-point-to-beginning)
  "Narrow to nearest class, return t on success, nil on failure."
  (interactive)
  (let ((p -1)
		(found-at nil))
	(save-excursion
	  (python-nav-beginning-of-block)
	  (while (and (not (= p (point)))
				  (not (my/py-statement-opens-class-p)))
		(setq p (point))
		(python-nav-backward-up-list))
	  ;; point now either at a non-class top-level, or class
	  (when (my/py-statement-opens-class-p)
		(setq found-at (point))
		(forward-line)
		(narrow-to-defun)))
	(if found-at
		(progn
		  (when move-point-to-beginning
			(goto-char found-at))
		  t)
	  nil)))

(defun +py--class-folded-p ()
  "Assuming we are at a class definition, check if the first
defun is folded."
  (save-excursion
	(beginning-of-line)
	(let ((col (current-column)))
	  (python-nav-forward-defun 2)
	  (when (and (not (= col (current-column)))
				 (hs-already-hidden-p))
		t))))

;; NOTE: +py--fold-class == (hs-hide-level 0)
(defun +py--fold-class ()
  "Assuming we are at a class definition, fold all of its
  methods."
  (save-excursion
	(beginning-of-line)
	(let ((start-col (current-column))
		  (last-point -1))
	  (python-nav-forward-defun 2)
	  (while (and (not (= (point) last-point))
				  (not (= start-col (current-column))))
		(hs-hide-block)
		(setq last-point (point))
		(python-nav-forward-defun)
		))))

;; TODO: make macro out of +py--fold-class and +py--unfold-class
(defun +py--unfold-class ()
  "Assuming we are at a class definition, unfold all of its
  methods."
  (save-excursion
	(beginning-of-line)
	(let ((start-col (current-column))
		  (last-point -1))
	  (python-nav-forward-defun 2)
	  (while (and (not (= (point) last-point))
				  (not (= start-col (current-column))))
		(hs-show-block)
		(setq last-point (point))
		(python-nav-forward-defun)
		))))

(defun +py--toggle-fold-class ()
  "Assuming we are at a class definition, toggle fold."
  (save-restriction
	(if (+py--class-folded-p)
		(+py--unfold-class)
	  (hs-hide-level 0))))

;; TODO: implement
(defun +py-hide-all ()
  "DWIM fold all top-level blocks."
  ;; TODO: do not, as hs-hide-all, fold classes fully
  ;; NOTE: take care of comments
  )

;; TODO: implement
(defun +py-show-all ()
  "DWIM unfold all top-level blocks (and/or everything)")

(defun +py-toggle-fold-surrounding-context ()
  "Toggle the fold of the surrounding context i.e. class methods
inside a class. Or everything outside a class."
  (interactive)
  ;; TODO: make it so that point-on-screen before = point-on-screen afterwards
  (save-restriction
	(save-excursion
	  (if (my/python-narrow-to-class t)
		  (+py--toggle-fold-class)
		(fn-hs-toggle-all))))

  ;; (save-restriction
  ;; (c-narrow-to-most-enclosing-decl-block)
  ;; (if (local-variable-p 'hidden)
  ;; 	(if (equal hidden t)
  ;; 		(progn
  ;; 		  (hs-show-all)
  ;; 		  (setq hidden nil))
  ;; 	  (progn
  ;; 		(hs-hide-all)
  ;; 		(setq hidden t)))
  ;;   (progn
  ;; 	(set (make-local-variable 'hidden) nil)
  ;; 	(hs-show-all))))
  )

(provide 'lang/python/folding)
