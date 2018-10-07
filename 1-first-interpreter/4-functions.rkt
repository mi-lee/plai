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
