
;;; ---------- EASY-BIND - Easy local binding for Common Lisp ----------
;;
;;
;; Version 1.0
;; Copyright (C) Marius Gaarde 2018. All rights reserved. Licensed under the MIT license.
;;
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
;; INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE
;; AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
;; DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 
;;
;; 

(in-package :edraag.easy-bind)

(defvar *verbose* t)

(defun test-easy-bind ()
  (let+ x = 1
	y = 2
	z = 3
	u = 4
	v = 5
	w = 6
	(a (b c) d &rest e) = (list x (list y z) u v w)
	(k l) = e
	f = (cons k l)
	(h . j) = f
	x = 60
	y = 70
	(z . u) = '(80 . 90)
	;; Body of let+ starts here
	(assert  (= (car e) k v 5))
	(assert	 (= (cadr e) l w 6))
	(assert	 (null (cddr e)))
	(assert	 (= b 2))
	(assert	 (= c 3))
	(assert	 (= a 1))
	(assert	 (= d 4))
	(assert  (= (+ x y z u) 300))
	(when *verbose* (format t "Let+ seems "))
	(when *verbose* (format t "to be "))
	(when *verbose* (format t "working~%"))
	(when *verbose* (format t "Should be the same: (5 . 6) and ~a and ~a~%" f (cons h j)))
	
	(let- u = v
	      v = u
	      x = y
	      y = x
	      (assert (= v 90))
	      (assert (= u 5))
	      (assert (= y 60))
	      (assert (= x 70))
	      (when *verbose* (format t "Let- seems to be working~%")))
	
	(multi-let (m n b) = (values x y z)
		   (a1 a2) = (floor (exp 1))
		   e = (+ a1 a2)
		   c = u
		   (a3 a4 a5 a6 a7) = (values 1 2 3 (list 4) (list 5))
		   c2 = (apply '+ a3 a4 a5 (append a6 a7))
		   ;; Body of multi-let starts here
		   (assert (= 300 (+ m n b c)))
		   (assert (= c2 15))
		   (when *verbose* (format t "Should be the same: ~f ~f~%" e (exp 1)))
		   (when *verbose* (format t "Multi-let seems "))
		   (when *verbose* (format t "to be "))
		   (when *verbose* (format t "working.~%")))
		   
	(letfun    n = 13
		   (square x) = (let- square = (* x x)
				      (when *verbose* (format t "Squaring ~a, " x))
				      (when *verbose* (format t "result = ~a ~%" square))
				      square)
		   m = (square n)
		   n = v
		   (fact n) = (case n 
				(0 1)
				(t (* n (fact (1- n)))))
		   (add &rest args) = (apply #'+ args)
		   (f x) = (add x m)
		   list = '(1 2 3 4 5 6 7 8 9 10)
		   newlist = (mapcar #'f list)
		   
		   (random-list n max) = (loop for i below n collect (random max))
		   random-list = (random-list 20 100)
		   (qsort list) = (letmatch list
				    () => ()
				    (p . xs) =>
				    (nconc (qsort (remove-if (lambda (x) (> x p)) xs))
					   (list p)
					   (qsort (remove-if-not (lambda (x) (> x p)) xs))))
	      
		   sorted-list = (qsort random-list)
		   (count-list list) = (letmatch list
					 () => 0
					 (_ . tail) => (+ 1 (count-list tail)))
		   length = (count-list sorted-list)
		   
		   (last-elt l) = (letmatch l
				    (x . nil) => x
				    (_x . xs) => (last-elt xs))
		   last-elt = (last-elt sorted-list)
		   
		   (sum-list l) = (letmatch l
				    () => 0
				    (x . xs) => (+ x (sum-list xs)))
		   sum-list = (sum-list sorted-list)
		   
		   (remove-dups l) = (letmatch l
					 () => ()
					 (x) => (list x)
					 (x y . ys) => (if (equal x y)
							   (remove-dups (cons y ys))
							   (cons x (remove-dups (cons y ys)))))
		   no-duplicate-sorted-list = (remove-dups sorted-list)
		   
		   (is-odd n)  = (if (zerop n) nil
				     (is-even (1- n)))
		   (is-even n) = (if (zerop n) t
				     (is-odd (1- n)))
	      
		   closure = (letfun count  = 0
				     (incr) = ((incf count)
					       (when *verbose* 
						 (format t "Closure: The count is now: ~a ~%" count))
					       count)
				     #'incr)
		   
		   ;; Body of letfun starts here
		   (letfun- (f x) = (* x 100)
			    (g x) = (f x)
			    (square x) = (let- square = (* x x)
					       (when *verbose* (format t "Another square function... ~%" ))
					       (when *verbose* (format t "Squaring away... ~%" ))
					       (when *verbose* (format t "(square ~a) = ~a ~%" x square))
					       square)
			    (assert (= 300 (f 3)))
			    (assert (= 170 (g 1)))
			    (assert (= 25 (square n)))
			    (when *verbose* (format t "Letfun- seems to be working~%")))
		   
		   (assert (is-odd 5))
		   (assert (is-even 6))
		   (assert (not (is-even 7)))
		   (assert (not (is-odd 8)))
		   (assert (= (funcall closure) 1))
		   (assert (= (funcall closure) 2))
		   (assert (= (funcall closure) 3))
		   (assert (= (nth 6 newlist) 176))
		   (assert (= 120 (fact n)))
		   
		   (loop for i from 1 below (length sorted-list) do
			(assert (>= (elt sorted-list i)
				    (elt sorted-list (- i 1)))))
		   
		   (loop for i from 1 below (length no-duplicate-sorted-list) do
			(assert (not (= (elt no-duplicate-sorted-list i)
					(elt no-duplicate-sorted-list (- i 1))))))
		   
		   (when *verbose* (format t "sorted-list = ~a ~%" sorted-list))
		   (when *verbose* (format t "no-duplicate-sorted-list = ~a ~%" no-duplicate-sorted-list))
		   (when *verbose* (format t "last-elt = ~a ~%" last-elt))
		   (when *verbose* (format t "sum-list = ~a ~%" sum-list))
		   (assert (= length (length sorted-list)))
		   (assert (= last-elt (nth (1- length) sorted-list)))
		   (assert (= sum-list (reduce '+ sorted-list)))
		   
		   (letfun (square x) = ((when *verbose* (format t "Inner square now...~%")
					       (format t "Not actually squaring ~d...~%" x))
					 x)
			   (assert (= (square 2) 2))
			   (assert (= x 60)))
		   (assert (= (square 2) 4))
		   (assert (= x 60))
		   (when *verbose* (format t "Letfun seems "))
		   (when *verbose* (format t "to be "))
		   (when *verbose* (format t "working.~%")))
	
	(letmacro  s = z
		   (macro-square x) =
		   (let+ g = (gensym)
			 `(let+ ,g = ,x
				(* ,g ,g)))
		   (macro-add &rest args) = ((format nil "Going to add up some args...")
					     (format nil "This will not be printed..." )
					     `(+ ,@args))
		   x = 5
		   y = (macro-square (macro-add x s))
		   
		   ; Example borrowed from blog post by Michael Malis
		   ; https://malisper.me/zap/
		   (zap fn place &rest args) =
		   (multi-let (temps exprs stores store-expr access-expr) =
			      (get-setf-expansion place)
			      `(let* (,@(mapcar #'list temps exprs)
				      (,(car stores)
				       (funcall ,fn ,access-expr ,@args)))
				 ,store-expr))
		   w = (zap #'+ x 5)
		   z = (zap #'* w 5)
		   ;; Body of letmacro starts here
		   (assert (= 50 z))
		   (assert (= 7225 y))
		   (when *verbose* (format t "Letmacro seems "))
		   (when *verbose* (format t "to be "))
		   (when *verbose* (format t "working.~%")))
	
	(letsym    x = (when *verbose* (format t "x expanded.~%"))
		   y = (when *verbose* (format t "y expanded.~%"))
		   z = (when *verbose* (format t "z expanded.~%"))
		   a = 8 b = 9
		   sum = (+ a b)
		   avg = (/ sum 2)
		   ;; Body of letsym starts here
		   x y z
		   (assert (= 17 sum))
		   (assert (= 17/2 avg))
		   (when *verbose* (format t "Letsym seems "))
		   (when *verbose* (format t "to be "))
		   (when *verbose* (format t "working if x, y, z expanded.~%" )))
	
	(let+      alist = '((:a 1) (:b 2) (:c 3))
		   tree  = '(+ (* 3 (+ 4 (/ 13 2)) 5) 14 (/ (- 23 (* 2 (+ 1 2))) 3))
		   match =  (letmatch alist
			      () => (error "~a not supposed to be empty!" alist)
			      x  => ((when *verbose* (format t "matched x~%")) x)
			      (x)       => ((when *verbose* (format t "matched (x)~%"))
					    (list x))
			      (x y)     => ((when *verbose* (format t "matched (x y)~%"))
					    (list x y))
			      (x . y)   => ((when *verbose* (format t "matched (x . y)~%"))
					    (list x y))
			      T => (error "Something is wrong with letmatch"))
		   
		   match2 = (letmatch alist
			      () => (error "~a not supposed to be empty!" alist)
			      (x)       => ((when *verbose* (format t "matched (x)~%"))
					    (list x))
			      (x y)     => ((when *verbose* (format t "matched (x y)~%"))
					    (list x y))
			      (x y z)   => ((when *verbose* (format t "matched (x y z)~%"))
					    (list x y z))
			      (x y z w) => ((when *verbose* (format t "matched (x y z w)~%"))
					    (list x y z w))
			      (x . y)   => ((when *verbose* (format t "matched (x . y)~%"))
					    (list x y))
			      T => (error "Something is wrong with letmatch"))
		   
	
	           match3 = (letmatch tree
			      () => (error "~a not supposed to be empty!" tree)
			      (x) => ((when *verbose* (format t "matched x~%")) x)
			      (_ (y z _ _) a (_ _ d)) => (list y z a d)
			      (x . y) => ((when *verbose* (format t "matched (x . y)~%"))
					    (list x y))
			      T => (error "Something is wrong with letmatch"))
	
		   (when *verbose* (format t "match  = ~a~%" match))
		   (when *verbose* (format t "match2 = ~a~%" match2))
		   (when *verbose* (format t "match3 = ~a~%" match3))
		   (when *verbose* (format t "(eval match3) = ~a~%" (eval match3)))
		   (assert (= 3 (length match)))
		   (assert (= 3 (length match2)))
		   (assert (= 4 (length match3)))
		   (assert (= 126 (eval match3)))
		   (when *verbose* (format t "Letmatch seems to be working~%")))
	
	(assert (= x 60))
	(let- x = 42
	      (assert (= x 42))
	      (let+ x = 1
		    (assert (= x 1)))
	      (assert (= x 42)))
	(assert (= x 60))
	(when *verbose* (format t "Binding constructs working OK.~%" ))
        t))

(defun test-easy-bind-silent ()
  (let- *verbose* = nil
	(test-easy-bind)))
