#lang plai-typed

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)])

(define (desugar [a : ArithS]) : ArithC
  (type-case ArithS a
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [multS (l r) (multC (desugar l) (desugar r))]))

(test (desugar (numS 5))
      (numC 5))
(test (desugar (plusS (numS 5) (numS 7)))
      (plusC (numC 5) (numC 7)))
(test (desugar (bminusS (numS 7) (numS 5)))
      (plusC (numC 7) (multC (numC -1) (numC 5))))
(test (desugar (multS (numS 5) (numS 7)))
      (multC (numC 5) (numC 7)))

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
              
