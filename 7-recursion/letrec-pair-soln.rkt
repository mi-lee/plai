#lang plai-typed
(require plai-typed/s-exp-match)

;; Start with "letrec.rkt", but make
;; a too-early use of a variable raise an
;; error, instead of returning 42

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol)
         (body : ExprC)
         (env : Env)]
  [pairV (f : Value)
         (s : Value)]
  )

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [plusC (l : ExprC) 
         (r : ExprC)]
  [multC (l : ExprC)
         (r : ExprC)]
  [lamC (n : symbol)
        (body : ExprC)]
  [appC (fun : ExprC)
        (arg : ExprC)]
  [if0C (tst : ExprC)
        (thn : ExprC)
        (els : ExprC)]
  [letrecC (n : symbol) 
           (rhs : ExprC)
           (body : ExprC)]
  [pairC (f : ExprC)
         (s : ExprC)]
  [fstC (pair : ExprC)]
  [sndC (pair : ExprC)])

(define-type Binding
  [bind (name : symbol)
        (val : (boxof (optionof Value)))])

(define-type-alias Env (listof Binding))

(define mt-env empty)
(define extend-env cons)

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
    [(s-exp-match? '{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (appC (lamC (s-exp->symbol (first bs))
                   (parse (third (s-exp->list s))))
             (parse (second bs))))]
    [(s-exp-match? '{lambda {SYMBOL} ANY} s)
     (lamC (s-exp->symbol (first (s-exp->list 
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? '{letrec {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (letrecC (s-exp->symbol (first bs))
                (parse (second bs))
                (parse (third (s-exp->list s)))))]
    [(s-exp-match? '{if0 ANY ANY ANY} s)
     (if0C (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
    [(s-exp-match? '{pair ANY ANY} s)
     (pairC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? '{fst ANY} s)
     (fstC (parse (second (s-exp->list s))))]
    [(s-exp-match? '{snd ANY} s)
     (sndC (parse (second (s-exp->list s))))]
    [(s-exp-match? '{ANY ANY} s)
     (appC (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

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
  (test (parse '{lambda {x} 9})
        (lamC 'x (numC 9)))
  (test (parse '{let {[x {+ 1 2}]}
                  y})
        (appC (lamC 'x (idC 'y))
              (plusC (numC 1) (numC 2))))
  (test (parse '{if0 1 2 3})
        (if0C (numC 1) (numC 2) (numC 3)))
  (test (parse '{letrec {[x {+ 1 2}]}
                  y})
        (letrecC 'x (plusC (numC 1) (numC 2))
                 (idC 'y)))
  (test (parse '{double 9})
        (appC (idC 'double) (numC 9)))
  (test/exn (parse '{{+ 1 2}})
            "invalid input"))

;; interp ----------------------------------------
(define (interp [a : ExprC] [env : Env]) : Value
  (type-case ExprC a
    [numC (n) (numV n)]
    [idC (s) (lookup s env)]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]
    [lamC (n body)
          (closV n body env)]
    [appC (fun arg) (type-case Value (interp fun env)
                      [closV (n body c-env)
                             (let ([val (interp arg env)])
                               (interp body
                                       (extend-env
                                        (bind n (box (some val)))
                                        c-env)))]
                      [else (error 'interp "not a function")])]
    [if0C (tst thn els)
          (type-case Value (interp tst env)
            [numV (n) (if (zero? n)
                          (interp thn env)
                          (interp els env))]
            [else (error 'interp "not a number")])]
    [letrecC (n rhs body)
             (let ([b (box (none))])
               (let ([new-env (extend-env
                               (bind n b)
                               env)])
                 (begin
                   (set-box! b (some (interp rhs new-env)))
                   (interp body new-env))))]
    [pairC (f s)
           (pairV (interp f env) (interp s env))]
    [fstC (pair) (extract-pair pair 'f env)]
    [sndC (pair) (extract-pair pair 's env)]))

(define (extract-pair (pair : ExprC) (side : symbol) (env : Env)) : Value
  (let ([p (interp pair env)])
            (type-case Value p
              [pairV (f s)
                     (cond
                       [(symbol=? 'f side) f]
                       [(symbol=? 's side) s]
                       [else (error 'interp "unrecognized")])]
              [else (error 'interp "not a pair")])))

(module+ test
  (test (interp (parse '2) mt-env)
        (numV 2))
  (test/exn (interp (parse `x) mt-env)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (box (some (numV 9))
                                          )) mt-env))
        (numV 9))
  (test (interp (parse '{+ 2 1}) mt-env)
        (numV 3))
  (test (interp (parse '{* 2 1}) mt-env)
        (numV 2))
  (test (interp (parse '{+ {* 2 3} {+ 5 8}})
                mt-env)
        (numV 19))
  (test (interp (parse '{lambda {x} {+ x x}})
                mt-env)
        (closV 'x (plusC (idC 'x) (idC 'x)) mt-env))
  (test (interp (parse '{let {[x 5]}
                          {+ x x}})
                mt-env)
        (numV 10))
  (test (interp (parse '{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env)
        (numV 12))
  (test (interp (parse '{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env)
        (numV 5))
  (test (interp (parse '{{lambda {x} {+ x x}} 8})
                mt-env)
        (numV 16))
  (test (interp (parse '{if0 1 2 3})
                mt-env)
        (numV 3))
  (test (interp (parse '{if0 0 2 3})
                mt-env)
        (numV 2))

  (test (interp (parse '{letrec {[x 5]}
                          {+ x x}})
                mt-env)
        (numV 10))

  (test (interp (parse '{letrec {[fac
                                  {lambda {x}
                                    {if0 x
                                         1
                                         {* x {fac {+ x -1}}}}}]}
                          {fac 5}})
                mt-env)
        (numV 120))

  (test/exn (interp (parse '{1 2}) mt-env)
            "not a function")
  (test/exn (interp (parse '{+ 1 {lambda {x} x}}) mt-env)
            "not a number")
  (test/exn (interp (parse '{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env)
            "free variable")
  (test/exn (interp (parse '{if0 {lambda {x} x} 1 2})
                    mt-env)
            "not a number")

  ;; This book language uses 42 instead of of #<undefined>:
  (test/exn (interp (parse '{letrec {[x x]} x})
                mt-env)
        "too early")
  (test (interp (parse '{pair 1 2})
                mt-env)
        (pairV (numV 1) (numV 2)))
  (test (interp (parse '{fst {pair 1 2}})
                mt-env)
        (numV 1))
  (test (interp (parse '{snd {pair 1 2}})
                mt-env)
        (numV 2)))

;; num+ and num* ----------------------------------------
(define (num-op [op : (number number -> number)] [l : Value] [r : Value]) : Value
  (cond
   [(and (numV? l) (numV? r))
    (numV (op (numV-n l) (numV-n r)))]
   [else
    (error 'interp "not a number")]))
(define (num+ [l : Value] [r : Value]) : Value
  (num-op + l r))
(define (num* [l : Value] [r : Value]) : Value
  (num-op * l r))

(module+ test
  (test (num+ (numV 1) (numV 2))
        (numV 3))
  (test (num* (numV 2) (numV 3))
        (numV 6)))

;; lookup ----------------------------------------
(define (lookup [n : symbol] [env : Env]) : Value
  (cond
   [(empty? env) (error 'lookup "free variable")]
   [else (if (symbol=? n (bind-name (first env)))
            (let ([z(unbox (bind-val (first env)))])
              (type-case (optionof Value) z
                [none () (error 'lookup "too early")]
                [some (v) v]))
             (lookup n (rest env)))]))

(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (box (some (numV 8)))) mt-env))
        (numV 8))
  (test (lookup 'x (extend-env
                    (bind 'x (box (some (numV 9))))
                    (extend-env (bind 'x (box (some (numV 8)))) mt-env)))
        (numV 9))
  (test (lookup 'y (extend-env
                    (bind 'x (box (some (numV 9))))
                    (extend-env (bind 'y (box (some (numV 8)))) mt-env)))
        (numV 8)))
