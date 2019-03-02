;;; modules/feature/pm/tests/test-pm-list.el         -*- lexical-binding: t; -*-

(eval-and-compile
  (require 'ert nil t)
  (require 'ert "lib/ert"))

(defmacro sheq (expr val)
  `(should (equal ,expr ,val)))

(ert-deftest pm-list-set-replace ()
  (let ((lst1 (list (cons 'nil (list (cons 'var1 1)
									 (cons 'var2 2)))
					(cons 'cat1 (list (cons 'var3  3)
									  (cons 'var2  4))))))
	(let-alist lst1
	  (sheq .cat1.var3 3))

	(setf lst1 (pm-list-set-or-replace (list (cons 'var3 42)) 'cat1 lst1))
	(let-alist lst1
	  (sheq .cat1.var3 42))

	(setf lst1 (pm-list-set-or-replace (list (cons 'var1 1)(cons 'var3 5)) 'nil lst1))
	(let-alist lst1
	  (sheq .nil.var1 1)
	  (sheq .nil.var3 5)
	  (sheq .cat1.var3 42))

	(setf lst1 (pm-list-set-or-replace (list (cons 'var1 1) (cons 'var3 5)) 'cat2 lst1))
	(setq lst lst1)
	(let-alist lst1
	  (sheq .nil.var1 1)
	  (sheq .nil.var3 5)
	  (sheq .cat2.var1 1)
	  (sheq .cat2.var3 5)
	  (sheq .cat1.var3 42))))

(ert-deftest pm-list-delete ()
  (let ((lst1 (list (cons 'nil (list (cons 'var1 1)
									 (cons 'var2 2)))
					(cons 'cat1 (list (cons 'var3  3)
									  (cons 'var2  4))))))

   (setf lst1 (pm-list-delete (list 'var1 'var2 'var3) 'nil lst1))

   (let-alist lst1
	 (sheq .nil.var1 nil)
	 (sheq .nil.var2 nil)
	 (sheq .nil.var3 nil)
	 (sheq .cat1.var3 3)))
  )

(ert-deftest pm-list-get ()
  (let ((lst1 (list (cons 'nil (list (cons 'var1 1)
									 (cons 'var2 2)))
					(cons 'cat1 (list (cons 'var3  3)
									  (cons 'var2  4))))))
	(let-alist lst1
	  (sheq .nil (pm-list-get (list 'var1 'var2) 'nil lst1))
	  (sheq '(.cat1.var3) (pm-list-get (list 'var3)) lst1 ))))

(ert-deftest pm-list-merge ()
  (let (
		(lst1 (list (cons 'nil (list (cons 'var1 1)
									 (cons 'var2 2)
									 (cons 'var3 3)))
					(cons 'cat1 (list
								 (cons 'var1  1)
								 (cons 'var2  2)
								 (cons 'var3  3)))
					(cons 'cat2 (list
								 (cons 'var1 111)))))

		(lst2 (list (cons 'nil (list (cons 'var1 11)
									 (cons 'var3 33)))
					(cons 'cat1 (list (cons 'var2  22)
									  (cons 'var3  33)
									  (cons 'var4 44))))))

	(setf lst2 (pm-list-merge :from lst1 :to lst2
							  :vars (list 'var0 'var1 'var3) :category 'nil ))
	(let-alist lst2
	  (sheq .nil.var0 nil)
	  (sheq .nil.var1 1)
	  (sheq .nil.var3 3))

	(setf lst1 (pm-list-merge :from lst2 :to lst1
							  :category 'cat1))
	(let-alist lst1
	  (sheq .cat1.var1 1)
	  (sheq .cat1.var2 22)
	  (sheq .cat1.var3 33))

	(setf lst2 (pm-list-merge :from lst1 :to lst2))

	(setq lst lst1)
	(setq lstt lst2)

	(let-alist lst2
	  (sheq .cat2.var1 111)
	  (sheq .cat1.var1 1)
	  (sheq .cat1.var2 22)
	  (sheq .cat1.var3 33))))

(ert-deftest pm-list-conforms-to ()
  (let (
		(lst1 (list (cons 'nil (list (cons 'var1 1)
									 (cons 'var2 2)))
					(cons 'cat1 (list (cons 'var3 3)
									  (cons 'var2 4)))))
		)

	(sheq (pm-list-conforms-to nil
  							   lst1)
  		  t)

	(sheq (pm-list-conforms-to '(())
  							   lst1)
  		  t)

	(sheq (pm-list-conforms-to '((nil . ((var1 . 1) (var2 . 2)))
								 (cat1 . ((var3 . 3))))
							   lst1)
		  t)

	(sheq (pm-list-conforms-to '((nil . ((var1 . 1) (var2 . 3)))
								 (cat1 . ((var3 . 3))))
							   lst1)
		  nil)

	(sheq (pm-list-conforms-to '((nil . (var1 (var2 . 2)))
  								 (cat1 . (var2)))
  							   lst1)
  		  t)

	(sheq (pm-list-conforms-to '((nil))
  							   lst1)
  		  t)

	(sheq (pm-list-conforms-to '((cat1))
  							   lst1)
  		  t)

	(sheq (pm-list-conforms-to '((cat1) (nil . (var1)))
  							   lst1)
  		  t)

	(sheq (pm-list-conforms-to '((cat1 . (var5))) lst1)
  		  nil)

	(sheq (pm-list-conforms-to '((cat-non-ex . (var10))) lst1)
  		  nil)

	(sheq (pm-list-conforms-to '((cat-non-existant))
  							   lst1)
  		  nil)

	;; original lst has not been modified:
	(let-alist lst1
  	  (sheq (length lst1) 2)
  	  (sheq .nil.var1 1)
  	  (sheq .nil.var2 2)
  	  (sheq .cat1.var3 3)
  	  (sheq .cat1.var2 4)))

  )



