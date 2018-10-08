(print-only-errors)

(define-type ExprC
  [numC (n number?)]
  [idC (s symbol?)]
  [plusC (l ExprC?)
         (r ExprC?)]
  [multC (l ExprC?)
         (r ExprC?)]
  [appC (s symbol?)
        (arg ExprC?)]
  [with (n symbol?)
        (rhs ExprC?)
        (body ExprC?)]
  )

(define-type FunDefC
  [fdC (name symbol?)
       (arg symbol?)
       (body ExprC?)])

(test (interp (parse '{double 8})
              mt-env
              (list double-def))
      16)
(test (interp (parse '{quadruple 8})
              mt-env
              (list double-def quadruple-def))
      32)
(define (interp a env fds)
  (type-case ExprC a
    [numC (n) n]
    ; no longer free variable error.
    [idC (s) (lookup s env) ]
    [plusC (l r) (+ (interp l env fds) (interp r env fds))]
    [multC (l r) (* (interp l env fds) (interp r env fds))]
    [appC (s arg) (local [(define fd (get-fundef s fds))]
                    ; replcae subst with environment.

                    ; 1) because args is expression, need to interp that too.
                    ; once evaluated, we need to replace args as the new function's argument.
                    ; 2) now we can evaluate the body.
                    (interp (fdC-body fd)
                            ; different:
                            ; suppose {let y 2 } bad 10} => {bad x is +x y}. but y is a free variable. should have an error. if we do substituion, then y should have no effect. But if we defer, then we will call the bad function and then substitue when we shouldn't. therefore, we should not wrap the env with the finding.
                            (extend-env
                             (bind (fdC-arg fd)
                                   (interp arg env fds))
                             mt-env) ; therefore, no substitutions have applied, so leave it as an empty environment.
                            fds)
                    )]
    [with (n rhs body)
          ; always want to interp rhs beacuse it's an expression
          ; but need to put this into the environment. needs to be found to n.
          ;  i.e. {with {[x {+ 1 x}]}
          ; {+ x x}}})
          ; x = { + 1 x}
          ; (extend-env
          ;  (bind n (interp rhs env fds))
          ;  env)
          ; now we van interp the body.
          (interp body
                  (extend-env
                   (bind n (interp rhs env fds))
                   env)
                  fds)
          ]))


(module+ test
  (test (interp (parse '2) mt-env empty)
        2)
  (test/exn (interp (parse `x) mt-env empty)
            "free variable")
  (test (interp (parse `x)
                (extend-env (bind 'x 9) mt-env)
                empty)
        9)
  (test (interp (parse '{+ 2 1}) mt-env empty)
        3)
  (test (interp (parse '{* 2 1}) mt-env empty)
        2)
  (test (interp (parse '{+ {* 2 3} {+ 5 8}})
                mt-env
                empty)
        19)
  (test (interp (parse '{double 8})
                mt-env
                (list double-def))
        16)
  (test (interp (parse '{quadruple 8})
                mt-env
                (list double-def quadruple-def))
        32)
  (test (interp (parse '{with {[x 5]}
                              {+ x x}})
                mt-env
                empty)
        10)
  (test (interp (parse '{with {[x 5]}
                              {with {[x {+ 1 x}]}
                                    {+ x x}}})
                mt-env
                empty)
        12)
  (test (interp (parse '{with {[x 5]}
                              {with {[y 6]}
                                    x}})
                mt-env
                empty)
        5)
  (test/exn (interp (parse '{with {[y 5]}
                                  {bad 2}})
                    mt-env
                    (list (parse-fundef '{define {bad x} {+ x y}})))
            "free variable"))


; TODO
; be able to define
; every identifier is one of these 3:
; binding
; bound
; free
