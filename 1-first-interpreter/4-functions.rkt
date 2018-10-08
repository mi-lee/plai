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

; Our  new Expression data definition:
(define-type ExprC
  [numC (n number?)]
  [idC (s symbol?)]
  [plusC (l ExprC?)
         (r ExprC?)]
  [multC (l ExprC?)
         (r ExprC?)]
  [appC (s symbol?)
        (arg ExprC?)])

(define-type FunDefC
  [fdC (name symbol?)
       (arg symbol?)
       (body ExprC?)])


; so if we have { + 1 2 } => (plusC (numC 1) (numC 2))

; what about {+ x 2} ? (plusC (idC 'x) (numC 2))
; why the quotes for quote-x? because x would be an expression in PLAI and it would be an unbound identifier. therefore we need the quote.

; what about {define {plus-two x} {+ x 2}} ?
(fdC 'plus-two
     'x
     (plusC (idC 'x)
            (numC 2)))

; what about {plus-two 9}?
(appC 'plus-two (numC 9))

; Last question:
{ define { double x } { + x x }}
{ define { quadruple x } { double { double x }}}
{ quadruple 2 }

(fdC 'double 'x (plusC (idC x) (idC x)))

(fdC 'quadruple
	'x
	(appC 'double (appC 'double (idC x))))

(appC 'quadrple (numC 2))


