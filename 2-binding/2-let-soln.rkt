#lang plai-typed
(require plai-typed/s-exp-match) ; install the plai-typed-s-exp-match package

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [plusC (l : ExprC) 
         (r : ExprC)]
  [multC (l : ExprC)
         (r : ExprC)]
  [appC (s : symbol)
        (arg : ExprC)]
  [letC (n : symbol) 
        (rhs : ExprC)
        (body : ExprC)])

(define-type FunDefC
  [fdC (name : symbol) 
       (arg : symbol) 
       (body : ExprC)])

;; An expr-S-exp is either
;; - number
;; - symbol
;; - (list '+ expr-S-expr expr-S-expr)
;; - (list '* expr-S-expr expr-S-expr)
;; - (list symbol expr-S-expr)
;; - (list 'let (list (list symbol expr-S-expr)) expr-S-expr)

;; A fundef-S-exp is
;; - (list 'define (list symbol symbol) arith-S-expr)

(module+ test
  (print-only-errors true))

;; parse ----------------------------------------
(define (parse [s : s-expression]) : ExprC
  (cond
    [(s-exp-match? `NUMBER s) (numC (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idC (s-exp->symbol s))]
    [(s-exp-match? '{+ ANY ANY} s)
     (plusC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? '{* ANY ANY} s)
     (multC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? '{SYMBOL ANY} s)
     (appC (s-exp->symbol (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [(s-exp-match? '{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (letC (s-exp->symbol (first bs))
             (parse (second bs))
             (parse (third (s-exp->list s)))))]
    [else (error 'parse "invalid input")]))

(define (parse-fundef [s : s-expression]) : FunDefC
  (cond
    [(s-exp-match? '{define {SYMBOL SYMBOL} ANY} s)
     (fdC (s-exp->symbol (first (s-exp->list (second (s-exp->list s)))))
          (s-exp->symbol (second (s-exp->list (second (s-exp->list s)))))
          (parse (third (s-exp->list s))))]
    [else (error 'parse-fundef "invalid input")]))

(module+ test
  (test (parse '2)
        (numC 2))
  (test (parse `x) ; note: backquote instead of normal quote
        (idC 'x))
  (test (parse '{+ 2 1})
        (plusC (numC 2) (numC 1)))
  (test (parse '{* 3 4})
        (multC (numC 3) (numC 4)))
  (test (parse '{+ {* 3 4} 8})
        (plusC (multC (numC 3) (numC 4))
               (numC 8)))
  (test (parse '{double 9})
        (appC 'double (numC 9)))
  (test (parse '{let {[x {+ 1 2}]}
                  y})
        (letC 'x (plusC (numC 1) (numC 2))
              (idC 'y)))
  (test/exn (parse '{{+ 1 2}})
            "invalid input")

  (test (parse-fundef '{define {double x} {+ x x}})
        (fdC 'double 'x (plusC (idC 'x) (idC 'x))))
  (test/exn (parse-fundef '{def {f x} x})
            "invalid input")

  (define double-def
    (parse-fundef '{define {double x} {+ x x}}))
  (define quadruple-def
    (parse-fundef '{define {quadruple x} {double {double x}}})))

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
                            fds))]
    [letC (n rhs body)
          (interp (subst (numC (interp rhs fds))
                         n
                         body)
                  fds)]))

(module+ test
  (test (interp (parse '2) empty)
        2)
  (test/exn (interp (parse `x) empty)
            "free variable")
  (test (interp (parse '{+ 2 1}) empty)
        3)
  (test (interp (parse '{* 2 1}) empty)
        2)
  (test (interp (parse '{+ {* 2 3} {+ 5 8}})
                empty)
        19)
  (test (interp (parse '{double 8})
                (list double-def))
        16)
  (test (interp (parse '{quadruple 8})
                (list double-def quadruple-def))
        32)
  (test (interp (parse '{let {[x 5]}
                          {+ x x}})
                empty)
        10))

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
    [appC (s arg) (appC s (subst what for arg))]
    [letC (n rhs body)
          (letC n
                (subst what for rhs)
                (if (symbol=? n for)
                    body
                    (subst what for body)))]))

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
  (test (subst (parse '8) 'x (parse '{let {[y x]} x}))
        (parse '{let {[y 8]} 8}))
  (test (subst (parse '8) 'x (parse '{let {[x x]} x}))
        (parse '{let {[x 8]} x})))
