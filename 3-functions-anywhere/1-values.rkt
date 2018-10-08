; note that (lambda (x) (+ x n)) has to remember where n came from.
; it's not because it's used during.. it's because it's a closure.
; for example, our function could return a function at the end.
; n is replaced with 7, independent whether n is on the stack or whatever.
;
; (let n 7 (lambda (x) (+ x n))) should also add 7 to things.



; now we're going to extend our grammar, but we have `lambda` (or fun)
; what's the result of lambda? if wew're not calling it, just return the function!
; so result is no longer a number
; result is now a "Value". a number OR a function.

; Ex 2
{let {y 10} {lambda {x} {+ x y}}}}
; what should this return? replace every y with 10, and now {lambda {x}{+ 10 x}

; Ex 3
; what about:
{let {f {lambda {x}{+ 1 x}}} {f 3}}
; we should set f to be a function
; then call apply!
{{lambda {x}{+ 1 x}} 3}
; but it doesn't match the grammar.
; so we should throw out the roll that function call is NAME-EXPR and now it is EXPR-EXPR.
; now it becomes:
; (use apply)
{{lambda {x}{+ 1 x}} 3}
(+ 1 3) ; 4
; application witht he argument.
; what if just
{ 1 2 }
; grammatically is now allowed... but you can't apply 1 and 2. so it should call an error. "not a function"
; what about
{+ 1 {lambda {x} 10}}
; error! not a number!
; 