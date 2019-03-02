;;; modules/feature/pm/pm-list.el                   -*- lexical-binding: t; -*-

;; functions to work on lists as stored in .dir-locals.el stashes,
;; i.e. files and caches.

(defun pm-list-set-or-replace (vars category lst)
  "Sets dotted pairs (var . val) in VARS in CATEGORY in provided
  LST. Existing variable values are replaced, new ones created.
  CATEGORY is created if it does not yet exist.."
  (when (not (-all? 'listp vars))
	(signal 'wrong-type-argument `("set-or-replace: vars must be a list of dotted pairs:" ,vars)))
  (let* ((lst (map-copy lst))
		 (category-vars (map-elt lst category)))
	(-map (-lambda ((var-name . var-value)) ; for every variable to be set/replaced...
			(let ((var (assoc var-name category-vars))) ; .. get it from the lst, if possible.
			  (if var
				  (setf (cdr var) var-value) ; update if it is present
				(if (null category-vars)
					(setf category-vars `((,var-name . ,var-value))) ; create the category if it does not exist
				  (nconc category-vars `((,var-name . ,var-value))) ; append the new variable if it is not
				  ))))
		  vars)
	(map-put lst category category-vars)
	lst))

(defun pm-list-delete (vars category lst)
  "Deletes symbols stored in VARS in CATEGORY in LST. "
  (when (not (-all? #'symbolp vars))
	(signal 'wrong-type-argument `("delete: vars must be a list of symbols" ,vars) ))

  (let* ((lst (map-copy lst))
		 (category-vars (map-elt lst category)))
	(when category-vars
	  (map-put lst category
			   (map-remove (lambda (var-present _) (-contains? vars var-present))
						   category-vars)))
	lst))

(defun pm-list-get (vars category lst)
  (let* ((category-vars (map-elt lst category))
		 (vars (-intersection vars (map-keys category-vars)))
		 out)
	(dolist (var vars)
	  (setf out (cons (assoc var category-vars) out)))
	(nreverse out)))

(cl-defun pm-list-merge (&key from to (vars -1) (category -1))
  "Merges VARS from CATEGORY FROM TO. When VARS are not
provided, all variables from CATEGORY are merged. When CATEGORY
is not provided, all CATEGORIES are merged."

  (when (not (listp from))
	(signal 'wrong-type-argument (list 'listp from)))
  (when (not (listp to))
	(signal 'wrong-type-argument (list 'listp to)))

  (let ((from (map-copy from))
		(to (map-copy to))
		(categories (if (eq category -1)
						(map-keys from)
					  (list category)))
		(all-vars (not (listp vars))))

	(-map (-lambda (category)
			(map-put to category
					 (if all-vars
						 (progn
						   (map-merge 'list
									  (map-elt to category)
									  (map-elt from category)))
					   (map-merge 'list
								  (map-elt to category)
								  (-filter (-lambda ((key . _)) (-contains? vars key))
										   (map-elt from category))))))
		  categories)
	to))

(cl-defun pm-list-sort (lst)
  "Sorts LST (or template) by category and variable."


  )

(cl-defun pm-list-conforms-to (template lst)
  "Checks if LST conforms to TEMPLATE. TEMPLATE can either
contain variable dotted-pairs, in which case the value of the
variable is checked also; variable names only, in which case only
the existence of the variable is checked, or nil, in which case
only the existence of a category is checked."

  ;; (when (not (listp from))
  ;; 	(signal 'wrong-type-argument (list 'listp from)))
  ;; (when (not (listp to))
  ;; 	(signal 'wrong-type-argument (list 'listp to)))

  (let ((template (copy-list template))
		(lst (copy-list lst))
		(categories (-intersection
					 (mapcar (lambda (el)
							   (if (listp el) (car el) el))
							 template)
					 (map-keys lst))))
	(cl-block pm-list-conforms-to
	  ;; all categories present?
	  (when (not (eq (length categories)
					 (length template)))
		(return-from pm-list-conforms-to nil))

	  ;; for every category in template ...
	  (dolist (cat-temp template)
		;; cat:  either (cat1) or (cat2 . var) or cat3
		(let* ((cat-sym (if (listp cat-temp) (car cat-temp) cat-temp))
			   (cat-temp-has-vars (and (listp cat-temp)
									   (not (null cat-temp)))))
		  (when cat-temp-has-vars
			(let ((cat-temp-vars 		; (var1 (var2 . 5)) etc.
				   (cdr cat-temp))
				  ;; ... find the corresponding category in lst, get variables ...
				  (cat-lst-vars			; ((var1 . 1) (var2 . 2) ...), or nil
				   (cdr (seq-find (lambda (cat-el)
									(if (eq (car cat-el) cat-sym) t nil))
								  lst))))
			  ;; ... and check if they match:
			  (dolist (var-temp cat-temp-vars)
				(let* ((var-sym (if (listp var-temp) (car var-temp) var-temp))
					   (var-temp-val (if (listp var-temp) (cdr var-temp) nil))
					   (var-lst-cons (assoc var-sym cat-lst-vars)))
				  (when (or (not var-lst-cons) ; variable not in lst
							(and var-temp-val
								 (not (eq var-temp-val (cdr var-lst-cons)))))
					;; variable value exists, and does not match
					(return-from pm-list-conforms-to nil))))))
		  ;; remove cat from lst
		  (setf lst (map-delete lst cat-sym))))
	  t)))
