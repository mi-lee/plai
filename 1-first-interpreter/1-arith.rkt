; our language: curly bracket: is OUR language we are implementing, not plai-typed

#lang plai

(print-only-errors)

; Arith "Core"
(define-type ArithC
  [numC (n number?)]
  [plusC (l ArithC?) (r ArithC?)]
  [multC (l ArithC?) (r ArithC?)])

; ArithC -> number
(define (interp a) ; : number
	(type-case ArithC a
		[numC (n) n]
		[plusC (left right) (+ (interp left) (interp right))]
		[multC (left right) (* (interp left) (interp right))]
		)
)


(test (interp (plusC (numC 2) (numC 1)))
	3)
(test (interp (numC 1))
	1)
(test (interp (multC (numC 2) (numC 3)))
	6)
(test (interp (multC (plusC (numC 5) (numC 8)) (numC 3)))
	39)
