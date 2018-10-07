#lang plai-typed

(print-only-errors #t)

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

;; An arith-S-exp is either
;; - number
;; - (list '+ arith-S-expr arith-S-expr)
;; - (list '* arith-S-expr arith-S-expr)

(define (parse [s : s-expression]) : ArithC
  (cond
    ; check if it s a number - if so, return it
    [(s-exp-number? s) (numC (s-exp->number s))]

    ; is it a list? AND is it length 3? AND is the first symbol "+"?
    [(and (s-exp-list? s)
          (= 3 (length (s-exp->list s)))
          (s-exp-symbol? (first (s-exp->list s)))
          (eq? '+ (s-exp->symbol (first (s-exp->list s)))))
     (plusC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]

    [(and (s-exp-list? s)
          (= 3 (length (s-exp->list s)))
          (s-exp-symbol? (first (s-exp->list s)))
          (eq? '* (s-exp->symbol (first (s-exp->list s)))))
     (multC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(test (parse '2)
      (numC 2))
(test (parse '{+ 2 1})
      (plusC (numC 2) (numC 1)))
(test (parse '{* 3 4})
      (multC (numC 3) (numC 4)))
(test (parse '{+ {* 3 4} 8})
      (plusC (multC (numC 3) (numC 4))
             (numC 8)))

(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]))

(test (interp (numC 2))
      2)
(test (interp (plusC (numC 2) (numC 1)))
      3)
(test (interp (multC (numC 2) (numC 1)))
      2)
(test (interp (plusC (multC (numC 2) (numC 3))
                     (plusC (numC 5) (numC 8))))
      19)

