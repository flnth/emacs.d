;; modules/ui/completion/config.el    -*- lexical-binding: t; -*-




;;;;; fuzzy completion

;; -> ui/completion

(defun completion-naive-fuzzy-completion (string table predicate point
                                                 &optional all-p)
  (let* ((string (s-replace " " "*" (s-collapse-whitespace string)))
		 (beforepoint (substring string 0 point))
         (afterpoint (substring string point))
         (boundaries (completion-boundaries beforepoint table predicate afterpoint))
         (prefix (substring beforepoint 0 (car boundaries)))
         (infix (concat
                 (substring beforepoint (car boundaries))
                 (substring afterpoint 0 (cdr boundaries))))
         (suffix (substring afterpoint (cdr boundaries)))
         ;; |-              string                  -|
         ;;              point^
         ;;            |-  boundaries -|
         ;; |- prefix -|-    infix    -|-  suffix   -|
         ;;
         ;; Infix is the part supposed to be completed by table, AFAIKT.
         (regexp (concat "\\`"
                         (mapconcat
                          (lambda (x)
                            (concat "[^" (string x) "]*?" (string x)))
                          infix
                          "")
                         ".*\\'"))
         (candidates (cl-remove-if-not
                      (apply-partially 'string-match-p regexp)
                      (all-completions prefix table predicate))))
    (if all-p
        ;; Implement completion-all-completions interface
        (when candidates
          ;; Not doing this may result in an error.
          (setcdr (last candidates) (length prefix))
          candidates)
      ;; Implement completion-try-completions interface
      (cond
       ((and (= (length candidates) 1)
             (equal infix (car candidates)))
        t)
       ((= (length candidates) 1)
        ;; Avoid quirk of double / for filename completion. I don't
        ;; know how this is *supposed* to be handled.
        (when (and (> (length (car candidates)) 0)
                   (> (length suffix) 0)
                   (char-equal (aref (car candidates)
                                     (1- (length (car candidates))))
                               (aref suffix 0)))
          (setq suffix (substring suffix 1)))
        (cons (concat prefix (car candidates) suffix)
              (length (concat prefix (car candidates)))))
       ;; Do nothing, i.e leave string as it is.
       (t (cons string point))))))

(defun completion-naive-fuzzy-try-completion (string table predicate point)
  (completion-naive-fuzzy-completion string table predicate point))
(defun completion-naive-fuzzy-all-completions (string table predicate point)
  (completion-naive-fuzzy-completion string table predicate point 'all))

(add-to-list 'completion-styles-alist
             '(naive-fuzzy
               completion-naive-fuzzy-try-completion
               completion-naive-fuzzy-all-completions
               "Simple naive-fuzzy completion, which never alters the string to complete, unless a unique match exists."))

(setq completion-styles
	  '(basic partial-completion substring naive-fuzzy))
;;;;; partial-completion:  treat SPC as *

;; -> dito!
(defun fn-completion-pcm--transform-string (orig-fun string table pred point &optional filter)
  (let ((string (s-replace " " "*" (s-collapse-whitespace string))))
	(apply orig-fun string table pred point filter)))
(advice-add 'completion-pcm--find-all-completions :around #'fn-completion-pcm--transform-string)
