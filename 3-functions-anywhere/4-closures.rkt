; what about
; {let y 10 {lambda x {+ y x}}} ?
; this substitution... it's deferred.
; so this will return {lambda {x} {+ x 10}}

; Ex 2
; {let y 10 {lambda {x} {+ y x}}
;{let y 7 y}}
; what is this?
; this is {lambda {x} {+ 10 x}} and then {7}
; so it should be lambda {x} {+ 10 x} {7} which should be 17.
(interp (let (y 7) 7)) => (interp y) then look up 7.
(interp (let y 10) (lambda (x) (+ y x)))
; should do something simliar
(interp (lambda (x) (+ 10 x))) => now it's a function....
; how to represent? It needs to REMEMEBER THAT Y = 10!
; how will it remember this value?
; we need to keep the environment bubble.
; put those 3 things together to be called  "Closure".

; so:

; a value is type...
; 1) numV: just the number inside
; 2) ClosureV : takes argument, body, and environemnt.

; a binding is type...
; 1) name and 2) value

; now:
; (interp 10 mt-env) => (numV 10)

; (interp (let y 10) (lambda (x) (+ y x))) mt-env)
; => should return
; (closureV 'x {+ y x} (extend-env (bind 'y (numV 10)) mt-env))



; how to do interp?
; - for num - return (numV n)
; - for id -- need to lookup
; - for plusC --need to interp left and right, but can't do + anymore, so we have to use
; (num+ function) so that it takes left and right values, ensure they are numbers, pull out the type (numV-n)
; - we can use num-op which will take op as an argument, and apply all the operations to each .

; - now we can use multC to use num*
; - what about let? no changes! why not? because we called interp and binding it..
; - what about lamC?

; lamC (n body) ... (interp body env) ...]
; - remember you can't evaluate it before it's called.
; lamC (n body)  (closV n body env) ] ; remember al of them.

; what about appC?
; [appC (fun arg) (.... (interp fun env) ..) ]
[appC (fun arg)
      ; it dpeneds what is each
      ; we exept it be a function value.
      (type-case Value (interp fun env)
        [closV (n body c-env)
               (interp body
                       ; env? don't forget to bind n from what we get from argument expression. wich env? c-env in type-case, or env at interp!
                       (extend-env (bind n (interp arg env)) c-env)
                       )

               ]
        [else (error "Not a function!")] ; can't apply {appC 1 5}
      ]
