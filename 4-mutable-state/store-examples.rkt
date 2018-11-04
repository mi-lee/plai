; interp: (ExprC Env Store -> Result)

; new box, add to store.
(test (interp (boxC (numC 5)) mt-env mt-store)
	(v*s (boxV 1) ; references address 1.
		; allocates address 1 to numV 5
		override-store
		 (cell 1 (numV 5))
		 mt-store)))


; so now:
(define-type Value
	[numV ...]
	[closV ...]
	[boxV (l: Location)] ; our own explicitly allocated location
	)


(test (interp (parse '{set-box! {box 5} 6))
	mt-env
	mt-store
	(v*s (numV 6)
		...
		...
		(override-store
			(cell 1 (numV 6))
			(override-store
				(cell 1 (numV 5))
				mt-store))
		...))
