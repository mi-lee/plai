(define (interp [a : ExprC] [env : Env] [sto : Store]) : Result
  (type-case ExprC a
    ; Now we have to return Result.
    [numC (n)
          (v*s (numV n))]
    [idC (s)
         ]
    [plusC (l r)
           ()]
    [multC (l r)
           ()]

    [lamC (n body)
          ]
    [appC (fun arg)
          ()]
    [boxC (a)
          ()]
    [unboxC (a)
            ()]
    [setboxC (bx val)
             ()]
    [beginC (l r)
            ()]))
