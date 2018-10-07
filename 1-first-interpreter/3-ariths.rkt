#lang plai

(print-only-errors)

; recall that
; interp: does the whole thing, from s-expression -> ArithS -> ArithC -> number
; 1) parse: S-expression to ArithS
; 2) desugar: ArithS to ArithC
; 3) interp does the rest

(define-type ArithC
  [numC (n number?)]
  [plusC (l ArithC?) (r ArithC?)]
  [multC (l ArithC?) (r ArithC?)]
  [id (name symbol?)])

(define-type ArithS
  [numS (n number?)]
  [plusS (l ArithS?) (r ArithS?)]
  [bminusS (l ArithS?) (r ArithS?)]
  [multS (l ArithS?) (r ArithS?)]
  [idS (name symbol?)])


; ===================================================
; from previously..
; ArithC -> number
(define *reserved-symbols* '(+ -))

;; valid-identifier? : any -> boolean
;; Determines whether the parameter is valid as an identifier name, i.e.,
;; a symbol that is not reserved.
(define (valid-identifier? sym)
  (and (symbol? sym)
       (not (member sym *reserved-symbols*))))

(define (interp a) ; : number
  (type-case ArithC a
    [numC (n) n]
    [id (name) (error "Error!")]
    [plusC (left right) (+ (interp left) (interp right))]
    [multC (left right) (* (interp left) (interp right))]
    )
)
; S expression => Arithmetic Expression
(define (parse sexp) ; returns Arithmetic Expression
  (match sexp
    [(? valid-identifier?) id] ; is it an ID?
    [(? number?) (numC sexp)] ; is it a number?
    [(list '+ lhs rhs) (plusC (parse lhs) (parse rhs))] ; checks if: is it a list with +, then some left and right hand side
    [(list '* lhs rhs) (multC (parse lhs) (parse rhs))] ; ditto but for *
    )
  )
; ===================================================


; subtraction is not fundamentally new. so we can "desugar" subtraction into re-using the addition implentation.
; "Desugaring" from ArithS to ArithC
; desugar: (ArithS -> ArithC)
; sort of like a compiler -- but input is very similar to output.

; ArithS -> ArithC
(define (desugar ariths)
  (type-case ArithS ariths
    [numS (n) (numC n)]
    [plusS (left right) (plusC (desugar left) (desugar right))]
    [bminusS (left right) (plusC (desugar left) (multC (numC -1) (desugar right)))]
    [multS (left right) (multC (desugar left) (desugar right))]
    [idS (name) (id name)]
  )
)

(test (desugar (numS 5))
      (numC 5))
(test (desugar (plusS (numS 5) (numS 7)))
      (plusC (numC 5) (numC 7)))
(test (desugar (bminusS (numS 7) (numS 5)))
      (plusC (numC 7) (multC (numC -1) (numC 5))))
(test (desugar (multS (numS 5) (numS 7)))
      (multC (numC 5) (numC 7)))


; Now time to try interp.
(test (interp (desugar (bminusS (numS 7) (numS 5)))) 2)
