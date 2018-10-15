#lang plai-typed
(require plai-typed/s-exp-match)

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol)
         (body : ExprC)
         (env : Env)]
  [recV (ns : (listof symbol))
        (vs : (listof Value))])

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [plusC (l : ExprC) 
         (r : ExprC)]
  [multC (l : ExprC)
         (r : ExprC)]
  [letC (n : symbol) 
        (rhs : ExprC)
        (body : ExprC)]
  [lamC (n : symbol)
        (body : ExprC)]
  [appC (fun : ExprC)
        (arg : ExprC)]
  [recordC (ns : (listof symbol))
           (args : (listof ExprC))]
  [getC (rec : ExprC)
        (n : symbol)]
  [setC (rec : ExprC)
        (n : symbol)
        (val : ExprC)])

(define-type Binding
  [bind (name : symbol)
        (val : Value)])

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
       (letC (s-exp->symbol (first bs))
             (parse (second bs))
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? '{lambda {SYMBOL} ANY} s)
     (lamC (s-exp->symbol (first (s-exp->list 
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]

    [(s-exp-match? '{record {SYMBOL ANY} ...} s)
     (recordC (map (lambda (l) (s-exp->symbol (first (s-exp->list l))))
                   (rest (s-exp->list s)))
              (map (lambda (l) (parse (second (s-exp->list l))))
                   (rest (s-exp->list s))))]
    [(s-exp-match? '{get ANY SYMBOL} s)
     (getC (parse (second (s-exp->list s)))
           (s-exp->symbol (third (s-exp->list s))))]
    [(s-exp-match? '{set ANY SYMBOL ANY} s)
     (setC (parse (second (s-exp->list s)))
           (s-exp->symbol (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
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
  (test (parse '{let {[x {+ 1 2}]}
                  y})
        (letC 'x (plusC (numC 1) (numC 2))
              (idC 'y)))
  (test (parse '{lambda {x} 9})
        (lamC 'x (numC 9)))
  (test (parse '{double 9})
        (appC (idC 'double) (numC 9)))
  (test (parse '{record {x 2} {y 3}})
        (recordC (list 'x 'y)
                 (list (numC 2) (numC 3))))
  (test (parse '{get {+ 1 2} a})
        (getC (plusC (numC 1) (numC 2)) 'a))
  (test (parse '{set {+ 1 2} a 7})
        (setC (plusC (numC 1) (numC 2)) 'a (numC 7)))
  (test/exn (parse '{{+ 1 2}})
            "invalid input"))

;; interp ----------------------------------------
(define (interp [a : ExprC] [env : Env]) : Value
  (type-case ExprC a
    [numC (n) (numV n)]
    [idC (s) (lookup s env)]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]
    [letC (n rhs body)
          (interp body
                  (extend-env
                   (bind n (interp rhs env))
                   env))]
    [lamC (n body)
          (closV n body env)]
    [appC (fun arg) (type-case Value (interp fun env)
                      [closV (n body c-env)
                             (interp body
                                     (extend-env
                                      (bind n
                                            (interp arg env))
                                      c-env))]
                      [else (error 'interp "not a function")])]
    [recordC (ns as)
             (recV ns
                   (map (lambda (a) (interp a env))
                        as))]
    [getC (a n)
          (type-case Value (interp a env)
            [recV (ns vs) (find n ns vs)]
            [else (error 'interp "not a record")])]
    [setC (a n v)
          (type-case Value (interp a env)
            [recV (ns vs)
                  (recV ns (update n (interp v env) ns vs))]
            [else (error 'interp "not a record")])]))

(module+ test
  (test (interp (parse '2) mt-env)
        (numV 2))
  (test/exn (interp (parse `x) mt-env)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (numV 9)) mt-env))
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

  (test (interp (parse '{record {a {+ 1 1}}
                                {b {+ 2 2}}})
                mt-env)
        (recV (list 'a 'b) 
              (list (numV 2) (numV 4))))
  (test (interp (parse '{get {record {a {+ 1 1}}
                                     {b {+ 2 2}}} a})
                mt-env)
        (numV 2))
  (test (interp (parse '{get {record {a {+ 1 1}}
                                     {b {+ 2 2}}} b})
                mt-env)
        (numV 4))
  (test (interp (parse '{set {record {a {+ 1 1}}
                                     {b {+ 2 2}}} a 5})
                mt-env)
        (recV (list 'a 'b) 
              (list (numV 5) (numV 4))))
  (test (interp (parse '{let {[r1 {record {a {+ 1 1}}
                                          {b {+ 2 2}}}]}
                          {let {[r2 {set r1 a 5}]}
                            {+ {get r1 a} {get r2 a}}}})
                mt-env)
        (numV 7))

  (test/exn (interp (parse '{1 2}) mt-env)
            "not a function")
  (test/exn (interp (parse '{+ 1 {lambda {x} x}}) mt-env)
            "not a number")
  (test/exn (interp (parse '{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env)
            "free variable"))

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
   [else (cond
          [(symbol=? n (bind-name (first env)))
           (bind-val (first env))]
          [else (lookup n (rest env))])]))

(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (numV 8)) mt-env))
        (numV 8))
  (test (lookup 'x (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'x (numV 8)) mt-env)))
        (numV 9))
  (test (lookup 'y (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'y (numV 8)) mt-env)))
        (numV 8)))
  
;; find & update ----------------------------------------

;; Takes a name and two parallel lists, returning an item from the
;; second list where the name matches the item from the first list.
(define (find [n : symbol] [ns : (listof symbol)] [vs : (listof Value)])
  : Value
  (cond
   [(empty? ns) (error 'interp "no such field")]
   [else (if (symbol=? n (first ns))
             (first vs)
             (find n (rest ns) (rest vs)))]))

;; Takes a name n, value v, and two parallel lists, returning a list
;; like the second of the given lists, but with v in place
;; where n matches the item from the first list.
(define (update [n : symbol] [v : Value]
                [ns : (listof symbol)] [vs : (listof Value)])
  : (listof Value)
  (cond
   [(empty? ns) (error 'interp "no such field")]
   [else (if (symbol=? n (first ns))
             (cons v (rest vs))
             (cons (first vs) 
                   (update n v (rest ns) (rest vs))))]))

(module+ test
  (test (find 'a (list 'a 'b) (list (numV 1) (numV 2)))
        (numV 1))
  (test (find 'b (list 'a 'b) (list (numV 1) (numV 2)))
        (numV 2))
  (test/exn (find 'a empty empty)
            "no such field")

  (test (update 'a (numV 0) (list 'a 'b) (list (numV 1) (numV 2)))
        (list (numV 0) (numV 2)))
  (test (update 'b (numV 0) (list 'a 'b) (list (numV 1) (numV 2)))
        (list (numV 1) (numV 0)))
  (test/exn (update 'a (numV 0) empty empty)
            "no such field"))


  

