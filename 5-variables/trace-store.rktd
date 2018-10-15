(interp (parse '[let {[x (box 5)]}
                  (begin
                    (set-box! x (lambda (y) x))
                    ((unbox x) 8))])
        mt-env
        mt-store)

... (interp (parse '{box 5})
            mt-env
            mt-store)

...... (interp (parse `5) mt-env mt-store)
     = (v*s (numV 5) mt-store)

    = (v*s (boxV 1) S1 = (override-store (cell 1 (numV 5)) mt-store))

... (interp (parse '(begin
                      (set-box! x (lambda (y) x))
                      ((unbox x) 8)))
            E1 = (extend-env (bind 'x (boxV 1) mt-env))
            S1)

...... (interp (parse 'set-box! x (lambda (y) x))
             E1
             S1)

......... (interp (parse 'x) E1 S1)
         = (v*s (boxV 1) S1)


......... (interp (parse '(lambda (y) x))
               E1
               S1)
          = (v*s (closV 'y (parse 'x) E1) S1)
       
[and then about 5 more steps]


    
                        

    