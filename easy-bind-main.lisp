
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
;; 

(in-package :edraag.easy-bind)

;; ------------ Helper functions for binding macros ------------

(defun multiple-of (n m)
  (zerop (mod m n)))

(defun equals-sign-p (x)
  (and (symbolp x)
       (string= (symbol-name x) "=")))

(defun equals-sign-or-being-p (x)
  (and (symbolp x)
       (or (string= (symbol-name x) "=")
	   (string= (symbol-name x) 'being))))

(defun simple-left-hand-side-p (x)
  (symbolp x))

(defun simple-or-complex-left-hand-side-p (x)
  (or (symbolp x) (consp x)))

(defun complex-left-hand-side-p (x)
  (consp x))

(defun parse-separated-list (forms left-hand-side-predicate separator-predicate)
  "Parses the forms only as long as pairs of forms are joined according to separator-predicate, 
like so: (a1 <separator> a2 b1 <separator> b2 ...), and a1, b1... satisfies left-hand-side-predicate.
Returns 2 values: 1) a list ((a1 a2) (b1 b2) ...), 2) the number of forms processed, including separators."
  (loop 
     with count       = 0
     and value-coming = nil
     and collected    = ()  ; becomes (... b2 b1 a2 a1)
     for elt in forms
     do (cond ((and (multiple-of 3 count)
		    (funcall left-hand-side-predicate elt))
	       (push elt collected)
	       (incf count))
	      ((funcall separator-predicate elt)
	       (setf value-coming t)
	       (incf count))
	      (value-coming
	       (push elt collected)
	       (incf count)
	       (setf value-coming nil))
	      (t (loop-finish)))
     finally
       (when value-coming
	 (error "Separator not followed by expression"))
       (when (oddp (length collected))
	 (setf collected (rest collected))
	 (decf count))
       (return (values 
		(loop for (a1 a2) on (nreverse collected) by #'cddr collecting (list a1 a2))
		count))))

(defun make-function-binding (binding)
  "Transforms binding ((fn x y) (do-stuff x y)) => (fn (x y) (do-stuff x y))."
  (list (caar binding) (cdar binding) (cadr binding)))

(defun collect-binding-list (bindings left-hand-side-predicate)
  "Takes a list ((a b) (c d) ...) and collects elems (x y) into new list only as long
as a, c, ... satisfies predicate."
  (let ((binding-list nil))
    (labels ((recur (bindings)
	       (cond
		 ((null bindings) nil)
		 ((funcall left-hand-side-predicate (caar bindings))
		  (push (car bindings) binding-list)
		  (recur (cdr bindings)))
		 (t nil))))
      (recur bindings))
    (nreverse binding-list)))

(defun generate-function-binding-list (bindings)
  (let ((function-bindings 
	 (collect-binding-list bindings (lambda (x) (and (complex-left-hand-side-p x)
							 (symbolp (car x))
							 (not (all-keyword-p (car x))))))))
    (loop for elt in function-bindings collect (make-function-binding elt))))

(defun generate-let-binding-list (bindings)
  (collect-binding-list bindings #'simple-left-hand-side-p))

(defun function-bindings-splice-implicit-progn (bindings)
  (loop for elt in bindings collect
       (single-splice-implicit-progn elt)))

(defun single-splice-implicit-progn (function-binding)
  (destructuring-bind (x y . z) function-binding
	 (if (and (consp (car z))
		  (consp (caar z))
		  (not (eq (caaar z) 'lambda)))
	     (list* x y (car z))
	     function-binding)))

(defun function-keyword-p (x)
  (eq x :fun))

(defun macro-keyword-p (x)
  (eq x :macro))

(defun values-keyword-p (x)
  (eq x :val))

(defun all-keyword-p (x)
  (eq x :all))

(defun sym-keyword-p (x)
  (eq x :sym))

(defun function-binding-p (x)
  (and (consp x)
       (function-keyword-p (car x))))

(defun macro-binding-p (x)
  (and (consp x)
       (macro-keyword-p (car x))))

(defun generate-let*s-and-complex-bindings (bindings body 
					    complex-binding-collector)
  "Generates a LET* form as long as it can collect simple bindings, and
complex bindings into a form or forms determined by complex-binding-collector, 
alternately until binding list exhausted, at which point body is spliced 
into the innermost form. Complex-binding-collector must be a function which 
takes the binding list as argument and returns 3 values: 1) a form-name, 
2) a list of bindings to give to the form, 3) the number of bindings it lays 
claim to."
  (labels ((recur (bindings body) 
	     (cond ((null bindings)
		    body)
		   ((simple-left-hand-side-p (caar bindings))
		    (let* ((let-bindings (generate-let-binding-list bindings))
			   (count (length let-bindings)))
		      `((let* ,let-bindings
			  ,@(recur (nthcdr count bindings) body)))))
		   ; Extra level of parens needed because body must be spliced in at the end of the recursion,
		   ; so nested let* and other forms must also be spliced. Thus the outer fn returns the
		   ; car of the list.
		   ((all-keyword-p (caaar bindings))
		    (setf (caar bindings) (cdaar bindings))
		    (destructuring-bind (x y) (car bindings)
		      (let ((value (gensym))
			    (value-list (gensym))
			    (number (length x)))
			`((let* ((,value ,y)
				 (,value-list (make-list ,number :initial-element ,value)))
			    (destructuring-bind ,x ,value-list
			      ,@(recur (cdr bindings) body)))))))
		    (t
		     (multiple-value-bind (form-name complex-binding-list count)
			 (funcall complex-binding-collector bindings)
		       (if (search (symbol-name 'bind) (symbol-name form-name))
			   `((,form-name ,@complex-binding-list
					 ,@(recur (nthcdr count bindings) body)))
			   `((,form-name ,complex-binding-list
					 ,@(recur (nthcdr count bindings) body)))))))))
    (car (recur bindings body))))

(defun let+-collect-function-bindings (bindings predicate)
  "Used to collect binding pairs where the first element is a list which begins with a
keyword like :fun or :macro, and transforming these to bindings which can be used by
forms like labels and macrolet. Collects bindings only as long as they satisfy predicate."
  (let ((function-bindings 
	 (collect-binding-list bindings predicate)))
    ; Get rid of initial keyword
    (loop for elt in function-bindings do
	 (setf (car elt) (cdar elt)))
    (loop for elt in function-bindings collect 
	 (make-function-binding elt))))

(defun collect-let+-complex-bindings (bindings)
  (let ((first-symbol (caaar bindings)))
    (cond
      ((function-keyword-p first-symbol)
       (let ((labels-bindings
	       (let+-collect-function-bindings bindings #'function-binding-p)))
	 (setf labels-bindings
	       (function-bindings-splice-implicit-progn labels-bindings))
	 (values 'labels labels-bindings (length labels-bindings))))
      
      ((macro-keyword-p first-symbol)
       (let ((macrolet-bindings
	      (let+-collect-function-bindings bindings #'macro-binding-p)))
	 (setf macrolet-bindings
	       (function-bindings-splice-implicit-progn macrolet-bindings))
	 (values 'macrolet macrolet-bindings (length macrolet-bindings))))
      
      ((values-keyword-p first-symbol)
       (setf (caar bindings) (cdaar bindings))
       (values 'multiple-value-bind (car bindings) 1))
      
      ((sym-keyword-p first-symbol)
       (setf (caar bindings) (cdaar bindings))
       (let ((symbol-macrolet-bindings 
	      (generate-symbol-macrolet-bindings (list (car bindings)))))
	 (values 'symbol-macrolet symbol-macrolet-bindings 1)))
      (t
       (values 'destructuring-bind (car bindings) 1)))))

(defun generate-let*s-and-function-bindings (bindings body form-name)
  (flet ((collector (bindings)
	   (let ((function-bindings 
		  (generate-function-binding-list bindings)))
	     (setf function-bindings (function-bindings-splice-implicit-progn function-bindings))
	     (values form-name function-bindings (length function-bindings)))))
    (generate-let*s-and-complex-bindings bindings body #'collector)))

(defun generate-let*s-and-labels (bindings body)
  (generate-let*s-and-function-bindings bindings body 'labels))

(defun generate-let*s-and-macrolets (bindings body)
  (generate-let*s-and-function-bindings bindings body 'macrolet))

(defun generate-let+-expansion (bindings body)
  (generate-let*s-and-complex-bindings bindings body 
				       #'collect-let+-complex-bindings))

(defun generate-let*s-and-multiple-value-binds (bindings body)
  "Used by multi-let to generate LET* and MULTIPLE-VALUE-BIND forms."
  (generate-let*s-and-complex-bindings bindings body
				       (lambda (bindings)
					 (values 'multiple-value-bind (car bindings) 1))))

(defun generate-symbol-macrolet-bindings (bindings)
  (loop for (left right) in bindings
     with collected = ()
     do 
       (progn (when (symbolp left)
		(push (list left right) collected))
	      (when (consp left)
		(if (null (rest left))
		    (push (list (first left) right) collected)
		    (if (and (consp right)
			     (= (length left) (length right)))
			(loop for i in left
			   for j in right
			   do (push (list i j) collected))
			(error "Malformed right-hand side in symbol-macro binding")))))
     finally (return (nreverse collected))))

;; ----------- Binding macros -----------

(defmacro define-binding-form (name &key 
				      left-hand-pred
				      separator-pred
				      form-generator)
  `(defmacro ,name (&rest forms)
     (multiple-value-bind
	(bindings count)
      (parse-separated-list forms 
			    ,left-hand-pred
			    ,separator-pred)
    (let ((body (nthcdr count forms)))
      (if (null bindings)
	  `(progn ,@body)
	  (funcall ,form-generator bindings body))))))

(define-binding-form let+ 
    :left-hand-pred #'simple-or-complex-left-hand-side-p 
    :separator-pred #'equals-sign-p
    :form-generator #'generate-let+-expansion)

(define-binding-form with
    :left-hand-pred #'simple-or-complex-left-hand-side-p 
    :separator-pred #'equals-sign-or-being-p
    :form-generator #'generate-let+-expansion)

(define-binding-form letval
    :left-hand-pred #'simple-or-complex-left-hand-side-p 
    :separator-pred #'equals-sign-p
    :form-generator #'generate-let*s-and-multiple-value-binds)

(define-binding-form let-
    :left-hand-pred  #'simple-left-hand-side-p 
    :separator-pred #'equals-sign-p
    :form-generator (lambda (bindings body) `(let ,bindings ,@body)))

(define-binding-form letfun
    :left-hand-pred #'simple-or-complex-left-hand-side-p 
    :separator-pred #'equals-sign-p
    :form-generator #'generate-let*s-and-labels)

(define-binding-form letfun-
    :left-hand-pred #'simple-or-complex-left-hand-side-p 
    :separator-pred #'equals-sign-p
    :form-generator (lambda (bindings body) 
		      `(flet ,(function-bindings-splice-implicit-progn
			       (generate-function-binding-list bindings)) 
			 ,@body)))

(define-binding-form letmacro
    :left-hand-pred #'simple-or-complex-left-hand-side-p 
    :separator-pred #'equals-sign-p
    :form-generator #'generate-let*s-and-macrolets)

(define-binding-form letsym
    :left-hand-pred #'simple-or-complex-left-hand-side-p 
    :separator-pred #'equals-sign-p
    :form-generator (lambda (bindings body)
		      `(symbol-macrolet 
			   ,(generate-symbol-macrolet-bindings bindings)
			 ,@body)))

(defmacro multi-let (&rest forms)
  `(letval ,@forms))
