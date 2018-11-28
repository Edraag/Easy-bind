
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

(defun matches (x y)
  (cond
    ((and (null x) (null y)) t)
    ((null x) nil)
    ((symbolp x) t)
    ((atom x) nil)
    ((consp y) (and (matches (car x) (car y))
		    (matches (cdr x) (cdr y))))
    (t nil)))

(defun consequent-sign-p (x)
  (and (symbolp x)
       (string= (symbol-name x) "=>")))

(defun letcond-body-check-wellformedness (forms)
  (loop with count = 0
     for elt in forms do
       (progn (incf count)
	      (when (and (multiple-of 3 (- count 2))
			 (not (consequent-sign-p elt)))
		(error "Malformed letcond expression - missing or misplaced => sign")))))

(defun splice-implicit-progn (forms)
  (loop for i below (length forms) collect
       (let+ (x . z) = (nth i forms)
	 (if (and (consp (car z))
		  (consp (caar z))
		  (not (eq (caaar z) 'lambda)))
	     (list* x (car z))
	     (nth i forms)))))

(defun structure-p (x)
  (or (atom x) (listp x)))

(defmacro letcond (key-expr &body body)
  "Conditionally expands into let+ form when list structure of key-expr
matches left-hand side of binding form in body."
  (letcond-body-check-wellformedness body)
  (let+ cond-clauses = (parse-separated-list body 
					     #'structure-p
					     #'consequent-sign-p)
	(setf cond-clauses (splice-implicit-progn cond-clauses))
	`(cond ,@(loop for clause in cond-clauses
		    collect (let+ car = (car clause)
				  cdr = (cdr clause)
				  `((matches ',car ,key-expr)
				    ,(if (and car (not (eq car t)))
					 `(let+ ,car = ,key-expr ,@cdr)
					 (car cdr))))))))
