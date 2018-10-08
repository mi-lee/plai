; Arithmetic + function => write in EBNF form
; new syntatic form: "with", local binding form
; {with {[x {+ 1 2}]} {+ x x }} => return 6

#lang plai

(print-only-errors)

(define-type ExprC
  [numC (n number?)]
  [idC (s symbol?)]
  [plusC (l ExprC?)
         (r ExprC?)]
  [multC (l ExprC?)
         (r ExprC?)]
  [with (n symbol?)
        (rhs ExprC?)
        (body ExprC?)]
  [appC (s symbol?)
        (arg ExprC?)])

; this is equivalent to...
(interp (parse '{with {[x 8]}
                      {+ x x}})
        ...)

(interp (subst (parse '8)
               'x
               (parse '{+ x x }))
        ...)

; need to extend substitution --

; 10 for x in {with {y 17} x} => {{with y 17} 10}
(test (subst (numC 10) 'x (with 'y (numC 17) (idC 'x)))
      (with 'y (numC 17) (numC 10))
      )

; what about 10 for x in {with {x y} x}?
; no! don't replace the first x, only replace the outside x - only the expressions!

(define (subst what for in)
  (type-case ExprC in
    ...
    [with (n rhs body)
    ; don't replace
          (with n
                (subst what for rhs)
                ; only sub body if symbol does NOT match
                ; only substitute body if symbol does match.
                ; for example, (subst 8 x (parse '{with {[y x]} x}))
                ; should result in (parse '{with {[y 8]} 8}))
                ; bt something like (subst 8 x (parse '{with {[x x]} x}))
                ; should result in (parse '{with {[x x]} 8})
                (if (symbol=? n for)
                    body ; just return it as it is, because we want it to to use the local binding
                    ; if they're different, then feel free to substitue
                    (subst what for body)
                    ))]
    ))


(module+ test
  (test (subst (parse '8) 'x (parse '9))
        (numC 9))
  (test (subst (parse '8) 'x (parse `x))
        (numC 8))
  (test (subst (parse '8) 'x (parse `y))
        (idC 'y))
  (test (subst (parse '8) 'x (parse '{+ x y}))
        (parse '{+ 8 y}))
  (test (subst (parse '8) 'x (parse '{* y x}))
        (parse '{* y 8}))
  (test (subst (parse '8) 'x (parse '{double x}))
        (parse '{double 8}))
  (test (subst (parse '8) 'x (parse '{with {[y x]} x}))
        (parse '{with {[y 8]} 8}))
  (test (subst (parse '8) 'x (parse '{with {[x x]} x}))
        (parse '{with {[x 8]} x})))
