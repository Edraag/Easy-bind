
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


(in-package "COMMON-LISP-USER")

;; ------------ Package definition for Easy-bind ------------

(defpackage :edraag.easy-bind
  (:use :common-lisp)
  (:nicknames :easy-bind)
  (:export :multi-let
	   :let+
	   :let-
	   :letfun
	   :letfun-
	   :letmacro
	   :letsym
	   :letmatch))
