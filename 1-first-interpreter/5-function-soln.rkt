#lang plai-typed

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [plusC (l : ExprC) 
         (r : ExprC)]
  [multC (l : ExprC)
         (r : ExprC)]
  [appC (s : symbol)
        (arg : ExprC)])

(define-type FunDefC
  [fdC (name : symbol) 
       (arg : symbol) 
          (body : ExprC)])

(module+ test
  (define double-def
    (fdC 'double 'x (plusC (idC 'x) (idC 'x))))
  (define quadruple-def
    (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))))

;; interp ----------------------------------------
(define (interp [a : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC a
    [numC (n) n]
    [idC (s) (error 'interp "free variable")]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]
    [appC (s arg) (local [(define fd (get-fundef s fds))]
                    (interp (subst (numC (interp arg fds))
                                   (fdC-arg fd)
                                   (fdC-body fd))
                            fds))]))

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


;; get-fundef ----------------------------------------
(define (get-fundef [s : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "undefined function")]
    [(cons? fds) (if (eq? s (fdC-name (first fds)))
                     (first fds)
                     (get-fundef s (rest fds)))]))

(module+ test
  (test (get-fundef 'double (list double-def))
        double-def)
  (test (get-fundef 'double (list double-def quadruple-def))
        double-def)
  (test (get-fundef 'double (list quadruple-def double-def))
        double-def)
  (test (get-fundef 'quadruple (list quadruple-def double-def))
        quadruple-def)
  (test/exn (get-fundef 'double empty)
            "undefined function"))

;; subst ----------------------------------------

(define (subst [what : ExprC] [for : symbol] [in : ExprC])
  (type-case ExprC in
    [numC (n) in]
    [idC (s) (if (eq? for s)
                 what
                 in)]
    [plusC (l r) (plusC (subst what for l)
                        (subst what for r))]
    [multC (l r) (multC (subst what for l)
                        (subst what for r))]
    [appC (s arg) (appC s (subst what for arg))]))

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
