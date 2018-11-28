# Easy-bind

Easy-bind is a collection of macros that make the binding of local variables, functions and macros 
in Common Lisp more convenient. In particular it reduces/automates the nesting resulting from the 
combined use of let/let* forms with other binding forms like destructuring-bind, multiple-value-
bind and flet/labels/macrolet. It adopts the "paren-free" equational syntax for binding local 
variables from the loop facility, and generalizes it to a simple domain-specific language for local 
binding.
