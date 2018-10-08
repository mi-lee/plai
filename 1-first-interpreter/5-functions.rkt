; Function representation
; How do we interp and evaluate function calls?


#lang plai

; 1) look up helper - the name "double"
; 2) substitute something like "3" and substitute it back
; functions:
; interp: ExprC (listof FunDefC) -> number
; get-fundef: (symbol (listof FundefC) -> FundefC)
; subst: (ExprC symbol ExprC -> ExprC)
; - what subst does: Take expression to plug in, symbol to express, target to plug into, and the result
; example: (subst {3} x (plusC (idC 'x) (idC 'x'))) => should result in (plusC (numC 3) (numC 3))


(print-only-errors)

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

(module+ test
  (define double-def
    (fdC 'double 'x (plusC (idC 'x) (idC 'x))))
  (define quadruple-def
    (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))))

(define (get-fundef s fds) ; : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "undefined function")]
    [(cons? fds) (if (eq? s (fdC-name (first fds)))
                     (first fds)
                     (get-fundef s (rest fds)))]))

;; interp ----------------------------------------
(define (interp a fds) ; : number
  (type-case ExprC a
    [numC (n) n]
    [idC (s) (error 'interp "free variable")] ; just an exception
    [plusC (left right) (+ (interp left fds) (interp right fds))]
    [multC (left right) (* (interp left fds) (interp right fds))]
    [appC (s arg) (local [(define fd (get-fundef s fds))]
                    (interp (subst (numC (interp arg fds)) ; 3 --> need to turn it back into an expression
                                   (fdC-arg fd)   ; x
                                   (fdC-body fd))
                            fds))])) ; (appC 'dobule ....')

(define (subst what for in)
  (type-case ExprC in ; only care about in, because we need to recurse in.
    [numC (n) in]
    [idC (s) (if (eq? s for)
              what ; retur new expression
              in)]
    [plusC (left right) (plusC (subst what for left)
                               (subst what for right))]
    [multC (left right) (multC (subst what for left)
                               (subst what for right))]
    [appC (s arg) (appC s (subst what for arg))]
    ))


(module+ test
  (test (subst (numC 8) 'x (numC 9))
        (numC 9))
  (test (subst (numC 8) 'x (idC 'x))
        (numC 8))
  (test (subst (numC 8) 'x (idC 'y))
        (idC 'y))
  (test (subst (numC 8) 'x (plusC (idC 'x) (idC 'y)))
        (plusC (numC 8) (idC 'y)))
  (test (subst (numC 8) 'x (multC (idC 'y) (idC 'x)))
        (multC (idC 'y) (numC 8)))
  (test (subst (numC 8) 'x (appC 'double (idC 'x)))
        (appC 'double (numC 8))))


(module+ test
  (test (interp (numC 2) empty)
        2)
  (test/exn (interp (idC 'x) empty)
            "free variable")
  (test (interp (plusC (numC 2) (numC 1)) empty)
        3)
  (test (interp (multC (numC 2) (numC 1)) empty)
        2)
  (test (interp (plusC (multC (numC 2) (numC 3))
                       (plusC (numC 5) (numC 8)))
                empty)
        19)
  (test (interp (appC 'double (numC 8))
                (list double-def))
        16)
  (test (interp (appC 'quadruple (numC 8))
                (list double-def quadruple-def))
        32))







