#lang plai

(print-only-errors)

; Concrete vs. Abstract Syntax
; we need representations from { + 2 1 } => (plusC (numC 2) (numC 1)).

; Concrete syntax as S-expression
; if we put ' quote mark ==> it becomes a Racket expression, a VALUE, and is not evaluated.
; parse function takes quoted form, takes concrete syntax => our representation of the syntax
; S-expression is either:
; - a number
; - a list that starts with +
; - a list that starts with *

(define-type ArithC
  [numC (n number?)]
  [plusC (l ArithC?) (r ArithC?)]
  [multC (l ArithC?) (r ArithC?)]
  [id (name symbol?)]
  )

(define *reserved-symbols* '(+ -))

;; valid-identifier? : any -> boolean
;; Determines whether the parameter is valid as an identifier name, i.e.,
;; a symbol that is not reserved.
(define (valid-identifier? sym)
  (and (symbol? sym)
       (not (member sym *reserved-symbols*))))

;; Reserved symbols.
(test (valid-identifier? '+) false)
(test (valid-identifier? '-) false)
;; Not a symbol
(test (valid-identifier? '{+ 1 2}) false)
(test (valid-identifier? 3) false)
(test (valid-identifier? "id") false)
;; OK
(test (valid-identifier? 'id) true)
(test (valid-identifier? 'x) true)



; S expression => Arithmetic Expression
(define (parse sexp) ; returns Arithmetic Expression
  (match sexp
    [(? valid-identifier?) id] ; is it an ID?
    [(? number?) (numC sexp)] ; is it a number?
    [(list '+ lhs rhs) (plusC (parse lhs) (parse rhs))] ; is it a list with +, then some left and right hand side?
    [(list '* lhs rhs) (multC (parse lhs) (parse rhs))] ; ditto but for *?
    )
  )

; tests
(test (parse '2)
      (numC 2))
(test (parse '{ + 1 2 })
      (plusC (numC 1) (numC 2)))
(test (parse '2)
      (numC 2))
(test (parse '{+ 2 1})
      (plusC (numC 2) (numC 1)))
(test (parse '{* 3 4})
      (multC (numC 3) (numC 4)))
(test (parse '{+ {* 3 4} 8})
      (plusC (multC (numC 3) (numC 4))
             (numC 8)))



; (test (interp (numC 2))
;       2)
; (test (interp (plusC (numC 2) (numC 1)))
;       3)
; (test (interp (multC (numC 2) (numC 1)))
;       2)
; (test (interp (plusC (multC (numC 2) (numC 3))
;                      (plusC (numC 5) (numC 8))))
;       19)

