
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

;; ------- A simple infix reader showing the use of destructuring in a with form -------

(defparameter *previous-readtables* nil)
(defparameter *infix-syntax*        nil)

;; First version - without letmatch
(defun read-infix (input-stream char)
  (declare (ignore char))
  (with list = (read-delimited-list #\] input-stream t)
	(&optional arg1 operator arg2 &rest rest) = list
	(cond ((null list)     ())
	      ((null operator) arg1)
	      ((null arg2)     (list operator arg1))
	      ((null rest)     (list operator arg1 arg2))
	      (t
	       (with rest being (rest list) 
		     rest-operands being
		     (loop
			for (symbol operand) on rest by #'cddr
			unless operand do
			  (error "Malformed infix expression - operand must follow operator")
			unless (eq symbol operator) do
			  (error "Malformed infix expression - only one operator allowed")
			collect operand)
		     (list* operator arg1 rest-operands))))))

;; Read-infix using letmatch to pattern-match the input list
(defun read-infix (input-stream char)
  (declare (ignore char))
  (with list = (read-delimited-list #\] input-stream t)
	(letmatch list 
	  ()                   => ()
	  (x)                  => x
	  (arg1 operator)      => (list operator arg1)
	  (arg1 operator arg2) => (list operator arg1 arg2)
	  (arg1 . rest) =>
	  (with operator being (first rest)
		rest-operands being
		(loop
		   for (symbol operand) on rest by #'cddr
		   unless operand do
		     (error "Malformed infix expression - operand must follow operator")
		   unless (eq symbol operator) do
		     (error "Malformed infix expression - only one operator allowed")
		   collect operand)
		(list* operator arg1 rest-operands)))))

(defun enable-infix ()
  "Enable infix expressions like [1 + 2 + 3]."
  (unless *infix-syntax*
    (push *readtable* *previous-readtables*)
    (setq *readtable* (copy-readtable))
    (set-macro-character #\[ 'read-infix)
    (set-syntax-from-char #\] #\))
    (setq *infix-syntax* t)))

(defun disable-infix ()
  (when *infix-syntax*
    (setq *readtable* (pop *previous-readtables*))
    (setq *infix-syntax* nil)))

;; ------- Simple quicksort implementation -------

;; With local function (closes over p)
(defun qsort (list)
  (when list
    (with (p . xs) = list
	  (:fun filter x) = (> x p)
	  (nconc (qsort (remove-if #'filter xs))
		 (list p)
		 (qsort (remove-if-not #'filter xs))))))

;; With symbol-macro - use with caution
(defun qsort (list)
  (when list
    (with (p . xs) = list
	  (:sym filter) = (lambda (x) (> x p))
	  (nconc (qsort (remove-if filter xs))
		 (list p)
		 (qsort (remove-if-not filter xs))))))

;; ------- Square root algorithm from SICP -------

(defun fixed-point (f start) 
  (letfun
   tolerance = 0.00001
   (iter old new) = (if (close-enuf old new)
			new
			(iter new (funcall f new)))
   (close-enuf u v) = (< (abs (- u v)) tolerance)
   (iter start (funcall f start))))

(defun square-root (x)
  (with
   (:fun f y) = (/ (+ y (/ x y)) 2)
   (fixed-point #'f 1.0)))
