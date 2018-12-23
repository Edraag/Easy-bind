
;;; ---------- EASY-BIND - Easy local binding for Common Lisp ----------
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

;; ------------ Helper functions for binding macros ------------

(defun multiple-of (n m)
  (zerop (mod m n)))

(defun equals-sign-p (x)
  (and (symbolp x)
       (or (string= (symbol-name x) "=")
	   (string= (symbol-name x) (symbol-name 'be))
	   (string= (symbol-name x) (symbol-name 'being)))))

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
	 (collect-binding-list bindings #'complex-left-hand-side-p)))
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

(defun generate-let*s-and-function-bindings (bindings body form-name)
  "Used by letfun and letmacro to generate LET* and LABELS/MACROLET forms, nested as needed to 
preserve order of evaluation.
Form-name = CL form which expects function-like bindings."
  (labels ((recur (bindings body)
	     (cond ((null bindings)
		    body)
		   ((simple-left-hand-side-p (caar bindings))
		    (let* ((let-bindings (generate-let-binding-list bindings))
			   (count (length let-bindings)))
		      `((let* ,let-bindings
			  ,@(recur (nthcdr count bindings) body)))))
		   ; Extra level of parens needed because body must be spliced in at the end of the recursion,
		   ; so nested let* and <form-name> forms must also be spliced. Thus the outer fn returns the
		   ; car of the list.
		   (t
		    (let* ((function-bindings (generate-function-binding-list bindings))
			   (count (length function-bindings)))
		      (setf function-bindings
			    (function-bindings-splice-implicit-progn function-bindings))
		      `((,form-name ,function-bindings
				    ,@(recur (nthcdr count bindings) body))))))))
    (car (recur bindings body))))

(defun function-keyword-p (x)
  (and (symbolp x)
       (string= (symbol-name x) (symbol-name 'fun))))

(defun macro-keyword-p (x)
  (and (symbolp x)
       (string= (symbol-name x) (symbol-name 'macro))))

(defun values-keyword-p (x)
  (and (symbolp x)
       (string= (symbol-name x) (symbol-name 'val))))

(defun all-keyword-p (x)
  (and (symbolp x)
       (string= (symbol-name x) (symbol-name 'all))))

(defun sym-keyword-p (x)
  (and (symbolp x)
       (string= (symbol-name x) (symbol-name 'sym))))

(defun function-binding-p (x)
  (and (consp x)
       (function-keyword-p (car x))))

(defun macro-binding-p (x)
  (and (consp x)
       (macro-keyword-p (car x))))

(defun collect-function-bindings (bindings predicate)
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

(defun generate-let*s-and-complex-bindings (bindings body)
  "Used by let+ to generate LET* and DESTRUCTURING-BIND/MULTIPLE-VALUE-BIND/LABELS/MACROLET 
forms, nested as needed to preserve order of evaluation."
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
		   (t
		    (let ((first-symbol (caaar bindings)))
		      (cond
			((function-keyword-p first-symbol)
			 (let* ((labels-bindings
				 (collect-function-bindings bindings #'function-binding-p))
				(count (length labels-bindings)))
			   (setf labels-bindings
				 (function-bindings-splice-implicit-progn labels-bindings))
			   `((labels ,labels-bindings
			       ,@(recur (nthcdr count bindings) body)))))
		      
			((macro-keyword-p first-symbol)
			 (let* ((macrolet-bindings
				 (collect-function-bindings bindings #'macro-binding-p))
				(count (length macrolet-bindings)))
			   (setf macrolet-bindings
				 (function-bindings-splice-implicit-progn macrolet-bindings))
			   `((macrolet ,macrolet-bindings
			       ,@(recur (nthcdr count bindings) body)))))
		      
			((values-keyword-p first-symbol)
			 (setf (caar bindings) (cdaar bindings))
			 `((multiple-value-bind ,@(car bindings)
			       ,@(recur (cdr bindings) body))))
			
			((sym-keyword-p first-symbol)
			 (setf (caar bindings) (cdaar bindings))
			 `((symbol-macrolet ,(generate-symbol-macrolet-bindings (list (car bindings)))
			     ,@(recur (cdr bindings) body))))
			
			((all-keyword-p first-symbol)
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
			 `((destructuring-bind ,@(car bindings)
			       ,@(recur (cdr bindings) body))))))))))
    (car (recur bindings body))))

(defun generate-let*s-and-labels (bindings body)
  (generate-let*s-and-function-bindings bindings body 'labels))

(defun generate-let*s-and-macrolets (bindings body)
  (generate-let*s-and-function-bindings bindings body 'macrolet))

(defun generate-let*s-and-<binding-name> (bindings body binding-name)
  "Generates LET* forms and <binding-name> forms, LET* forms nested as needed to preserve order of evaluation."
  (labels ((recur (bindings body) 
	     (cond ((null bindings)
		    body)
		   ((simple-left-hand-side-p (caar bindings))
		    (let* ((let-bindings (generate-let-binding-list bindings))
			   (count (length let-bindings)))
		      `((let* ,let-bindings
			  ,@(recur (nthcdr count bindings) body)))))
		   ; Extra level of parens needed because body must be spliced in at the end of the recursion,
		   ; so nested let* and <binding-name> forms must also be spliced. Thus the outer fn returns the
		   ; car of the list.
		   (t
		    `((,binding-name ,@(car bindings)
				     ,@(recur (cdr bindings) body)))))))
    (car (recur bindings body))))

(defun generate-let*s-and-multiple-value-binds (bindings body)
  "Used by multi-let to generate LET* and MULTIPLE-VALUE-BIND forms."
  (generate-let*s-and-<binding-name> bindings body 'multiple-value-bind))

(defun generate-symbol-macrolet-bindings (bindings)
  (loop for binding in bindings
     with collected = ()
     do (let ((car (car binding))
	      (cadr (cadr binding)))
	  (when (symbolp car)
	    (push binding collected))
	  (when (consp car)
	    (loop for i in car
	       for j in cadr
	       do (push (list i j) collected))))
     finally (return (nreverse collected))))

;; ----------- Binding macros -----------

(defmacro let+ (&rest forms)
  "Expands into LET* and DESTRUCTURING-BIND forms, and optionally into LABELS, MACROLET and
MULTIPLE-VALUE-BIND forms when appropriate keywords are given, or a PROGN if no bindings are
given. Forms nested as needed to preserve order of evaluation."
  (multiple-value-bind
	(bindings count)
      (parse-separated-list forms 
			    #'simple-or-complex-left-hand-side-p 
			    #'equals-sign-p)
    (let ((body (nthcdr count forms)))
      (if (null bindings)
	  `(progn ,@body)
	  (generate-let*s-and-complex-bindings bindings body)))))

(defmacro letval (&rest forms)
  "Expands into LET* and MULTIPLE-VALUE-BIND forms, LET*s nested if needed to preserve order of
 evaluation, or a PROGN form if no bindings are given."
  (multiple-value-bind 
	(bindings count) 
      (parse-separated-list forms 
			    #'simple-or-complex-left-hand-side-p 
			    #'equals-sign-p)
    (let ((body (nthcdr count forms)))
      (if (null bindings)
	  `(progn ,@body)
	  (generate-let*s-and-multiple-value-binds bindings body)))))

(defmacro let- (&rest forms)
  "Expands into a single LET form, or a PROGN if no bindings are given"
  (multiple-value-bind
	(bindings count)
      (parse-separated-list forms 
			    #'simple-left-hand-side-p 
			    #'equals-sign-p)
    (let ((body (nthcdr count forms)))
      (if (null bindings)
	  `(progn ,@body)
	  `(let ,bindings ,@body)))))

(defmacro letfun (&rest forms)
  "Expands into LET* and LABELS forms, nested if needed to preserve order of evaluation,
or a PROGN form if no bindings are given."
  (multiple-value-bind
	(bindings count)
      (parse-separated-list forms 
			    #'simple-or-complex-left-hand-side-p 
			    #'equals-sign-p)
    (let ((body (nthcdr count forms)))
      (if (null bindings)
	  `(progn ,@body)
	  (generate-let*s-and-labels bindings body)))))

(defmacro letfun- (&rest forms)
  "Expands into a single FLET form, or a PROGN if no bindings are given"
  (multiple-value-bind
	(bindings count)
      (parse-separated-list forms 
			    #'complex-left-hand-side-p 
			    #'equals-sign-p)
    (let ((body (nthcdr count forms)))
      (if (null bindings)
	  `(progn ,@body)
	  `(flet ,(function-bindings-splice-implicit-progn
		   (generate-function-binding-list bindings)) 
	     ,@body)))))

(defmacro letmacro (&rest forms)
  "Expands into LET* and MACROLET forms, nested if needed to preserve order of evaluation,
or a PROGN form if no bindings are given."
  (multiple-value-bind 
	(bindings count)
      (parse-separated-list forms 
			    #'simple-or-complex-left-hand-side-p 
			    #'equals-sign-p)
    (let ((body (nthcdr count forms)))
      (if (null bindings)
	  `(progn ,@body)
	  (generate-let*s-and-macrolets bindings body)))))

(defmacro letsym (&rest forms)
  "Expands into a single SYMBOL-MACROLET form, or a PROGN if no bindings are given"
  (multiple-value-bind
	(bindings count)
      (parse-separated-list forms 
			    #'simple-or-complex-left-hand-side-p 
			    #'equals-sign-p)
    (let ((body (nthcdr count forms)))
      (if (null bindings)
	  `(progn ,@body)
	  `(symbol-macrolet 
	       ,(generate-symbol-macrolet-bindings bindings)
	     ,@body)))))

(defmacro with (&rest forms)
  `(let+ ,@forms))

(defmacro multi-let (&rest forms)
  `(letval ,@forms))
