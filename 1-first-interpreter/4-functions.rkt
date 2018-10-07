; Function representation

#lang plai

(print-only-errors)

; what we want:
{ define { double x } { + x x }}
{ define { quadruple x } { double { double x }}}
{ quadruple 2 } ; should result in 8.

; something like this WOULDN'T make sense:
{ + {define {double x} {+ x x}} 1}
; Why not?
; function definition is not an expression.
; we can't add it to our current grammar of expressions.
; what about
{ + {double 4} 1} ; yes! why? because it's a function call/application, NOT definition.


; Therefore...
; a function has:
; 1) a name, 2) argument name/params, 3) body.
; look at the body...
; a BodyC could be made up of Nums, IDs, plus, mult. That looks just like ArithC!
; so we say that body is an expression (which means identifiers are also expressions.)
;
