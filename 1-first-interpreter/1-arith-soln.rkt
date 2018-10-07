#lang plai-typed

(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

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
