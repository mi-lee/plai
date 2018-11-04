# Functions as Values

Note that (lambda (x) (+ x n)) has to remember where n came from. It's not because it's used during... it's because it's a closure.


## Ex 1
For example, our function could return a function at the end:

n is replaced with 7, independent whether n is on the stack or whatever.

```racket
(let n 7 (lambda (x) (+ x n))) ;should also add 7 to things.
```

Now we're going to extend our grammar, but we have `lambda` (or fun).

What's the result of lambda? if wew're not calling it, just return the function!
So result is no longer a number - result is now a `Value`, a number OR a function.

## Ex 2

```racket
{let {y 10} {lambda {x} {+ x y}}}}
```
what should this return? replace every y with 10, and now {lambda {x}{+ 10 x}

## Ex 3

what about:

```racket
{let {f {lambda {x}{+ 1 x}}} {f 3}}
```

we should set f to be a function
then call apply!

## Ex 4

```racket
{{lambda {x}{+ 1 x}} 3}
```

but it doesn't match the grammar. so we should throw out the roll that function call is NAME-EXPR and now it is EXPR-EXPR. now it becomes:


## Ex 5
```
(use apply)
{{lambda {x}{+ 1 x}} 3}
(+ 1 3) 4
```

application with the argument.


## Ex 6

what if just

```racket
{ 1 2 }
```

grammatically is now allowed... but you can't apply 1 and 2. so it should call an error. "not a function"


## Ex 7

what about

```racket
{+ 1 {lambda {x} 10}}
```

error! not a number!


# Closures

## Ex 1

what about

```racket
{let y 10 {lambda x {+ y x}}} ?
```

this substitution... it's deferred.
so this will return `{lambda {x} {+ x 10}}`.

## Ex 2

```racket
{let y 10 {lambda {x} {+ y x}}
;{let y 7 y}}
```

what is this? This is `{lambda {x} {+ 10 x}}` and then `{7}`.

so it should be `lambda {x} {+ 10 x} {7}` which should be `17`.

```racket
(interp (let (y 7) 7)) => (interp y) then look up 7.
(interp (let y 10) (lambda (x) (+ y x)))
```

should do something simliar

```racket
(interp (lambda (x) (+ 10 x))) => now it's a function....
```
how to represent? It needs to **remember that Y = 10!**

how will it remember this value? we need to keep the environment bubble. put those 3 things together to be called  "Closure".

so:

## TLDR

a value is type...

1. numV: just the number inside
2. ClosureV : takes argument, body, and environemnt.

a binding is type...

1) name and
2) value


## Ex 3

now:
```
(interp 10 mt-env) => (numV 10)
(interp (let y 10) (lambda (x) (+ y x))) mt-env)
;=> should return
(closureV 'x {+ y x} (extend-env (bind 'y (numV 10)) mt-env))
```


## How to construct the interpreter?

- for num - return (numV n)
- for id -- need to lookup
- for plusC --need to interp left and right, but can't do + anymore, so we have to use
(num+ function) so that it takes left and right values, ensure they are numbers, pull out the type (numV-n)
- we can use num-op which will take op as an argument, and apply all the operations to each.

- now we can use multC to use num*
- what about let? no changes! why not? because we called interp and binding it..
- what about lamC?
```
lamC (n body) ... (interp body env) ...]
```

- remember you can't evaluate it before it's called.

```racket
lamC (n body)  (closV n body env) ] ;remember al of them.
```

- what about appC?
```
[appC (fun arg) (.... (interp fun env) ..) ]
[appC (fun arg)
      it dpeneds what is each
      we exept it be a function value.
      (type-case Value (interp fun env)
        [closV (n body c-env)
               (interp body
                       env? don't forget to bind n from what we get from argument expression. wich env? c-env in type-case, or env at interp!
                       (extend-env (bind n (interp arg env)) c-env)
                       )

               ]
        [else (error "Not a function!")] can't apply {appC 1 5}
      ]
```
