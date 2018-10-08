; Parsing let
; use match

; Substitution is costly.
; why?
; replace the x (the most inner one) into 1
; then
; can end up with O(n^2) in subsitution. Instead, attach each bubble with a box -- we are "deferring substitution" to avoid having to run it every time.
; at the end, then look up the env and
; instead of EAGERLY SUBSTIUTING, just defer it till the end.
; before (interp y) used to be an error. Now, we should look it up if y is within the set of deferred substitution.
;
; what about:
(interp {let {x 1}
          {let x 2}
          x})
; which x do we replace it with? the closest one!
; instead of taking an expression, it needs an ENVIRONMENT.
(interp): ExprC Env (listof FundDef) -> number
; we need helpers..
; mt-env: Type Env
; extend-env: Binding Env -> Env
; 	- new binding, add a new bubble
; bind: symbol number -> binding
; 	- i.e. x = 1
; lookup: symbol env -> number
; 	- take symbol, env, and returns a number.
(extend-env (bind 'x 1)
            (mt-env))

; for every single call, we have to (extend-env (bind 'y 2)
;(extend-env (bind 'x 1)
;	mt-env))))
