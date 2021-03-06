
;;; ---------- EASY-BIND - Easy local binding for Common Lisp ----------
;;
;; Version 1.1
;; Copyright (C) Marius Gaarde 2018. All rights reserved. Licensed under the MIT license.
;;
;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
;; BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
;; DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 
;;

(in-package :edraag.easy-bind)

(defun matches (x y)
  "Implements the rules for matching a left-hand side (x) to
a given key-expression (y) in a letmatch form."
  (cond
    ((and (null x) (null y)) t)
    ((null x) nil)
    ((symbolp x) t)
    ((atom x) nil)
    ((consp y) (and (matches (car x) (car y))
		    (matches (cdr x) (cdr y))))
    (t nil)))

(defun map-leaves (tree predicate replacement-fn)
  "Finds the leaves of the tree and conses up a similar tree 
where the leaves are replaced according to replacement-fn, but
only if they satisfy predicate."
  (cond ((null tree) nil)
	((consp tree) 
	 (cons (map-leaves (car tree) predicate replacement-fn)
	       (map-leaves (cdr tree) predicate replacement-fn)))
	(t (if (funcall predicate tree)
	       (funcall replacement-fn tree)
	       tree))))

(defun nmap-leaves (tree predicate action)
  "Non-consing version of map-leaves. Returns nil, or the
result of applying action if tree is an atom and satisfies
predicate."
  (cond ((null tree) nil)
	((consp tree) 
	 (map-leaves (car tree) predicate action)
	 (map-leaves (cdr tree) predicate action))
	(t (if (funcall predicate tree)
	       (funcall action tree)
	       nil))))

(defun consequent-sign-p (x)
  (and (symbolp x)
       (string= (symbol-name x) "=>")))

(defun ignorablep (x)
  (and (symbolp x)
       (char= (elt (symbol-name x) 0) #\_)))

(defun splice-implicit-progn (forms)
  "Remove outer list around right-hand sides of
arguments to letmatch."
  (loop for form in forms collect
       (with (x . z) = form
	     (if (and (consp (car z))
		      (consp (caar z))
		      (not (eq (caaar z) 'lambda)))
		 (list* x (car z))
		 form))))

(defun letmatch-body-check-wellformedness (forms)
  (loop with count = 0
     for elt in forms do
       (progn (incf count)
	      (when (and (multiple-of 3 (- count 2))
			 (not (consequent-sign-p elt)))
		(error "Malformed letmatch expression - missing or misplaced => sign")))))

(defmacro letmatch (key-expr &body body)
  "Conditional form of let+ which expands into a cond form where the test-forms are calls to
`matches' to check if the structure of each binding-list in the body matches that of key-expr.
The body should consist of pairs of binding-lists and forms separated by a fat-arrow (=>).
The first binding-list that matches is bound to key-expr in a let+ form (at runtime), its
right-hand form becoming the body of the let+ form."
  (letmatch-body-check-wellformedness body)
  (let+ cond-clauses = (parse-separated-list body 
					     #'simple-or-complex-left-hand-side-p
					     #'consequent-sign-p)
	(setf cond-clauses (splice-implicit-progn cond-clauses))
	`(cond ,@(loop for clause in cond-clauses collecting
		      (let+ car = (car clause)
			    cdr = (cdr clause)
			    ignorables = ()
			    (setf car (map-leaves car #'ignorablep
						  (lambda (x) (gensym (symbol-name x)))))
			    (nmap-leaves car #'ignorablep (lambda (x) (push x ignorables)))
			    (if (eq car t)
				`(t ,@cdr)
				`((matches ',car ,key-expr)
				  ,(if car
				       `(let+ ,car = ,key-expr 
					      (declare (ignore ,@ignorables)) 
					      ,@cdr)
				       (car cdr)))))))))
