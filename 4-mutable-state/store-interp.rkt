(define (interp [a : ExprC] [env : Env] [sto : Store]) : Result
  (type-case ExprC a
    [numC (n)
          ]
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
