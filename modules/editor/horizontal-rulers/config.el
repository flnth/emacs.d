;; modules/editor/horizontal-rulers/config.el    -*- lexical-binding: t; -*-

;; TODO: make package out of this, use use-package
;; TODO: extend to have dashes added to an comment line

(defvar fn--horizontal-rule-default-max-column 60)
(defvar fn--horizontal-rule-possible-max-columns '(60 80 120))

;;;###autoload
(defun fn-horizontal-rule-present? (&optional comment-prefix char)
  "Checks if a horizontal rule with a length of at least 6 is
present. Moves POINT of its beginning if so and returns its START
and END in a list, or NIL."
  (let* ((prefix (if comment-prefix comment-prefix
				   comment-start))
		 (char (if char char ?-))
		 (end 0))
	(when (s-contains? (concat prefix (make-string 6 char ))
					   (thing-at-point 'line t))
	  (move-end-of-line 1)
	  (setf end (point))
	  (search-backward prefix)
	  `(,(point) ,end))))

;;;###autoload
(defun fn-find-nearest-index (x lst)
  "Find the index of the element in LST that is nearest to X."
  (let* ((sub  (mapcar #'(lambda (el) (abs (- el x))) lst)))
	(min (seq-min sub))
	(cl-position (seq-min sub) sub)))

;;;###autoload
(defun fn-insert-horizontal-rule (&optional N comment-prefix char)
  "Inserts a horizontal ruler from point to column
fn--horizontal-rule-default-max-column, or N. If there already is
a ruler on the line, toggle its length."
  (interactive)
  (let* ((prefix (if comment-prefix comment-prefix comment-start))
		 (max-col (if N N fn--horizontal-rule-default-max-column))
		 (rule-length (- max-col (length prefix) (current-column) ))
		 (char (if char char ?-)))
	(insert prefix)
	(insert-char char rule-length)))

;;;###autoload
(defun fn-horizontal-rule (&optional char)
  "Inserts a horizontal rulers from point to column, or replaces
  the rule alreadly on the line with one of a different length."
  (interactive)
  (let* ((prefix (if (-contains? '(emacs-lisp-mode lisp-mode common-lisp-mode)
								 major-mode)
					 (concat ";;" comment-padding) ; lisp modes
				   (concat comment-start)))        ; all other modes
		 (char (if char char ?-))
		 (existing-ruler (fn-horizontal-rule-present? prefix char)))
	(if existing-ruler
		;; existing -> replace
		(let* ((rule-max-column (- (cadr existing-ruler) (car existing-ruler)))
			   (rule-length-known? (cl-position rule-max-column
												fn--horizontal-rule-possible-max-columns))
			   (nearest-index (fn-find-nearest-index rule-max-column
													 fn--horizontal-rule-possible-max-columns))
			   (new-rule-max-column (if rule-length-known?
										;; known length -> cycle to next
										(elt fn--horizontal-rule-possible-max-columns
											 (mod (+ 1 nearest-index) (length fn--horizontal-rule-possible-max-columns) ))
									  ;; unknown length -> set to nearest
									  (elt fn--horizontal-rule-possible-max-columns
										   nearest-index))))
		  (delete-region (car existing-ruler) (cadr existing-ruler))
		  (goto-char (car existing-ruler))
		  (fn-insert-horizontal-rule new-rule-max-column prefix char))
	  ;; non-existing -> insert default rule
	  (fn-insert-horizontal-rule fn--horizontal-rule-default-max-column prefix char))))


(add-hook #'prog-mode-hook #'(lambda ()
							   (spacemacs/set-leader-keys-for-major-mode major-mode "_" #'(lambda () (interactive) (fn-horizontal-rule ?-)))
							   (spacemacs/set-leader-keys-for-major-mode major-mode "=" #'(lambda () (interactive) (fn-horizontal-rule ?=)))))




