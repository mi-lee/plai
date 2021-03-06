# First Interpreter

<!-- MarkdownTOC -->

- [Intro to plai](#intro-to-plai)
- [First Interpreter](#first-interpreter)
  - [Why use `'` and `{}`?](#why-use--and-)
  - [`read`](#read)
  - [Desugaring?](#desugaring)
  - [What about implementing functions?](#what-about-implementing-functions)
    - [What is `id` for?](#what-is-id-for)
    - [What about function applications?](#what-about-function-applications)
  - [Now how can we interp function calls?](#now-how-can-we-interp-function-calls)
  - [How does `subst` work?](#how-does-subst-work)
- [Aside: Probabilistic programming](#aside-probabilistic-programming)

<!-- /MarkdownTOC -->



# Intro to plai

- Watch the videos by Matthew Flatt [here](https://pubweb.eng.utah.edu/~cs5510/schedule.html). They are excellent.

If you have done HtDD before, `define-type` from the PLAI library is essentially the same way to define a data definition (and we can use it for HtDF).

```racket
;; define-type turns the Data Driven Template Rules from comments into code.
;; sort of a "one-of + compound structure with named fields"

;; <PLAI-DT>  ::= (define-type <name> <variants>)
;;
;; <variants> ::= <empty>
;;              | <variant> <variants>
;;
;; <variant>  ::= [ <name> <fields> ]
;;
;; <fields>   ::= <empty>
;;            | <field> <fields>
;;
;; <field> ::= ( <name> <contract> )
```

For example,

```racket
(define-type Animal
  [snake (name: symbol)
         (weight: number)]
  [tiger (name: symbol)
         (stripe-count: number)])
```

And now we can use it as a template to create functions, such as below:

```racket
(define (heavy-animal? [a: Animal])
  (type-case Animal a
    [snake (name weight)
           .... name ... weight]
    [tiger (...name ...stripe-count)]
    )
  )
```



# First Interpreter

*PLAI from Chap 1-6.*


We will interpret something like `{+ 2 1}` or `{+ 2 {* 4 3}}`.

- curly brackets because it's the user input we're implementing (NOT plai)
- Since it takes both signs and number, we need to implement all 3 types (+, -, number)

```racket
#lang plai

(define-type AE
  [numC (n number?)]
  [add (left AE?) (right AE?)]
  [minus (left AE?) (right AE?)]
  )

; Let's create a very basic interpreter.
(define (interp anArithmeticExpr)
  (type-case AE anArithmeticExpr
    [numC (n) n] ; just return n as it is
    [add (left right) (+ (interp left) (interp right))]
    [minus (left right) (- (interp left) (interp right))]
    )
  )

(test (interp (numC 2)) 2)
(test (interp (add (minus (numC 2) (numC 3))
                   (numC 2))
  ) 1)
```

Now let's move onto parsing.

Now let's move onto parsing. Note that we are using (add (numC 2) (numC 3)) for plai language to represent the arithmetic language expression.

But we want {+ 2 1} to work! `'{+ 2 1} ` (a quote expresison in front): not evaluated, but is the shorthand for Racket's `read`.

So we want the parser to turn from our desired syntax (i.e. `'{+ 2 1`} to something like `(add (numC 2) (numC 1))`, and then our interpreter can take over from there.)

So we want something like this to pass:

```racket
(test (parse '{+ 2 1}) (plus (numC 2) (numC 1)))
```

so that eventually parse can hand over the syntax to interp, which does the work in actually interpreting it.
```
(test (interp (parse '{+ 3 4})) 7)
```

so what is required of parse?
```
(define (parse sExpression)
  ; is it a number? then return it as an s-expression
  ; is it a list with the "+" symbol?
  ; we can do this by (list '+ leftSexp rightSexp')
  [(list '+)]
  ; is it a valid identifier? i.e., is it a symbol
  []
  )
```

Now the following tests should work:
```
; (test (interp (parse '3)) 3)
; (test (interp (parse '{- 3 4})) -1)
; (test (interp (parse '{+ {- 3 4} 7})) 6)
; (test (interp (parse '{+ 7 {- 3 4}})) 6)
```

## Why use `'` and `{}`?

If we put ' quote mark ==> it becomes a Racket expression, a VALUE, and is not evaluated.

The curly braces are because we need representations from `{ + 2 1 }` => `(plusC (numC 2) (numC 1))`. The parse function takes quoted form, takes concrete syntax => our representation of the syntax.



## `read`

Using a quote, `(quote {+ 1 2})` or just `'{+ 1 2}'`, returns a constant called a datum, and is shorthand notation for `read`.

Racket's reader is a recursive-descent parser that can be configured easily.

- `( | [ | {` will lead to reading a list

Using `read` is identical to:

`(interp (parse '{+ {* 2 3} {+ 5 8}}'))`

to

```racket
(parse (read))

// Waits for user input..
```





## Desugaring?

We can "desugar" the `minus` by re-using the `plus`. For example, `desugar: (ArithS -> ArithcC)`, from a define-type WITH subtraction, to a define-type WITHOUT subtraction.

How would we implement a desugar, then?

```racket
(define (desugar sExpression) ; -> Returns a C expression
  (type-case AE sExpression
    [numS ..]
    [plusS ...]
    [bminusS (left right) (desugar left) (multiplyC (numC -1) (desugar right))] ; now we recursively desugar, but multiply the result of desugared-right with -1.
    )
)
```



## What about implementing functions?

Let's say we want something like

```racket
{define {double x}
	{+ x x}}
```

Note that we can't do something like

```racket
{+ {define {double x} {+ x x}} 1}
```

and why not? because functions are not expressions. We need to deal with function definitions.

A function **call** is an expression (also known as **function application**).

A function has:

1. Name
2. Argument name
3. Body

So now, expressions can be...

1. Identifier
2. Number
3. Addition s-expression
4. multiplication s-expression
5. ...
6. function-call expressions
7. AND function definition.



```racket
(define-type Expression
	[numC ...]
	[idC ...]
	[app (s symbol?) (arg Expression?)]
)

```racket
```
; and define our function definition here.
(define-type FunctionDefinition
	[fdC (name symbol?) (arg symbol?) (body Expression?)]
)
```

In the example of `double` above...

```racket
{define {double x}
	{+ x x}}

; name: "double"
; arg: "x"
; body: "{+ x x}
```

### What is `id` for?

Something like `{ + x 2}`, how would we deal with this? Since `x` is an identifier (that is NOT a symbol like `+`, which is considered reserved), we have to represent it like `(plusC (idC 'x) (numC 2))`.

Why is it quote-x? It's so it can represent the variable `x`.



So funded looks like this

```racket
(define-type FunDef
  [fundef (fun-name symbol?) ; for lookup
          (arg-name symbol?) ; know what to replcae with in the body
          (body F1WAE?)]) ; payload: when we call fcn, we bind it to the value. but in lazy, we would bind it to the UNEVALUATED value. In eager, we would bind it to the evaluated value.
; why those 3 parts?
; without argname, this is how you get variation in your function! (bc function is abstraction)
; the rest is essential.

```racket

And an example would be

```racket
; EXAMPLE
(fundef 'double
        'n
        (add (id 'n) (id 'n)))
```

to represent that the function `double`. It need to be wrapped with a quote to indicate that it is a symbol.



### What about function applications?

Remember that they are expressions (but function definitions are not). So something like

`{double 2}`

would need to represented as part of our `ArithmeticExpression` define-type, but as a function call/application. this is denoted by:

```racket
(define-type Expression
	[numC ...]
	[idC ...]
	[app (s symbol?) (arg Expression?)]
)

; (s symbol?) = "double"
; arg Expression? = "2"
```



## Now how can we interp function calls?

Take `double` as an example.

`{double 3}` steps:

1. Look up `double` in our function definitions and find the body.
2. Replace `double` with the substitution with `+` and the arguments.
3. Keep recursively evaluating.

So helpers needed are:

- Function lookup
- Substitutor
- and interpreter



`interp`: Give the interpreter the expression and list of function refs, and gives result.

`get-fundef`: gives the function definition

`subst`: takes expression to plug in, symbol, and expression we're plugging into => result.

For example,

`subst : (ExprC symbol ExprC -> ExprC)`

- Expr: `3`
- `symbol`: x
- `{plus}`

i.e. `subst( 3 x plusC ) -> (+ 3 x)`



How should `appC` work? it should be like

```racket
(interp (appC 'double (numC 8))
			(list double-def))
			16)

; appC: takes s-expression, returns...
; 1) function def from lookup,
; 2) body, we want to start interpreting but substitute into it. so call (subst .... body)
```



## How does `subst` work?

```racket
(define (subst [what: ExprC]
               [for: symbol]
               [in: Exprc])
               ...)
```



For example:

```racket
(test (subst (numC 8) 'x (idC 'x)))
; what are we replacing? (numC 8)
; for which symbol? ('x)
; and where are we substituting into? (idC 'x).
; Let's give another example.

(test (subst (numC 8) 'x (appC 'double (idC 'x)))
   (appC 'double (numC 8))))
```



# Aside: Probabilistic programming



A gentle introduction [here](https://www.youtube.com/watch?v=9SEIYh5BCjc).
