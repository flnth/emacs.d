;; -*- lexical-binding: t -*-

;; Provide mu4e-query-rewrite-function which
;;  - adds * after every word that is not inside quotes
;;    e.g. `string1 "string2 string3"' -> `string1* "string2 string3"*'

;; TODO:
;;  - gracefully handle parentheses, no * needed after those


(defvar fn--mu4e-query-rewrite-wildcard-exceptions '("and" "or" "not" "xor"
													 "AND" "OR" "NOT" "XOR"))

(defun fn--join-words-within-quotes (words)
  "Takes list of strings, and joins words that are within
double quotes."
  (let ((inside-quote nil)
		(out '()))
	(loop for w in words
		  if inside-quote
		  do (progn
			   (setf (car out) (s-append (concat " " w) (car out)))
			   (when (s-ends-with? "\"" w)
				 (setf inside-quote nil))
			   )
		  else
		  do (progn
			   (when (s-contains? "\"" w)
				 (setf inside-quote t))
			   (setf out (cons w out))))
	(nreverse out)))

(defun fn--extract-parens-contents-from-string (s)
  "...cannot deal with nested parens, for now (!)"
  (let ((inside-parens nil)
		(out '()))
	;; for every character
	;;    - when not inside parens, and ( detected, mark index1, inside parens true
	;;    - when inside parens, and ) detected, mark index2, inside parens false
	;;     -> remove [index1,index2] from string
	;;     -> add s[index1+1, index2-1]
	)
  )

(defun fn--join-words-within-parens (words)
  (let ((inside-parens nil)
		(out '()))
	(loop for w in words
		  if inside-parens
		  do (progn
			   (setf (car out) (s-append (concat " " w) (car out)))
			   (when (s-ends-with? ")" w)
				 (setf inside-parens nil))
			   )
		  else
		  do (progn
			   (when (s-starts-with? "$(" w)
				 (setf inside-parens t))
			   (setf out (cons w out))))
	(nreverse out)))

(defun fn--mu4e-query-rewrite-function (query)
  "Rewriting function to append a star "
  ;; TODO: extract sub-queries in parentheses, work on them separately, then
  ;; combine it all back together.
  ;; OR: just ignore everything in parens... or not. ^^.
  (let ((ret
		 (let* (
				(query-words (s-split " " query))
				;; (subqueries (fn--mu4e-query-rewrite-function ) (fn--join-words-within-parens query-words))
				(query-words (fn--join-words-within-parens (fn--join-words-within-quotes query-words))))
		   (message (format "%s" query-words))
		   (s-join " "
				   (loop for w in query-words collect
						 (if (or (s-suffix? "*" w)
								 (s-suffix? "\"" w)
								 (s-suffix? ")" w)
								 (member w fn--mu4e-query-rewrite-wildcard-exceptions))
							 w
						   (s-append "*" w)))))))
	(message ret)
	(if ret ret query)))

(setq mu4e-query-rewrite-function #'fn--mu4e-query-rewrite-function)
