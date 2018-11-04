## Mutable state



Motivation: substitution relies on an identifier having fixed value. but this is not true in plai.

`set!`: assignment operator. Then variables have "state" because particular value at particular time.

Other languages have it! Java, Python etc...

When is state essential? a GUI..



**State is a side channel** -- an extra way for communicating in different parts of a program! 

Pros: you can add new channels at will, but cons: channels of communication not apparent..



## Variables vs. boxes

begin: needed to sequence the result -- returns the value of the expression of the **last** one.



## Boxes as boxes

```scheme
(define-type Value
  [numV (n : number)]
  [closV (arg : symbol)
         (body : ExprC)
         (env : Env)]
  [boxV (b : (boxof Value))])	
```

Don't rely on Racket boxes.





### State - store

We can communicate the values explicitly.

Consider a 2d matrix where set-box will update the value, and unbox will return that value (according to some address).

So interp is now:

```
interp: (Expression Environment -> Value)
NOW:

interp: (Expression Environment Store -> Value)
```

needs to be passed in every time.

store is "memory"

You can change memory over time, so memory needs to be produced by `interp`. So instead of returning Value, needs to return Value AND Store, so instead we'll call it `Result`:

```scheme
(define-type Result
             [v*s (v: Value) (s: Store)])
```



#### Communicating the store

Now, instead of

```
(num+ (interp l env) (interp r env))
```

it needs a store to interp r.

1. We need a some starting state: `(interp l env) => (v * s ...)`
2. Then that state needs to be threaded to the next interp: `(interp r env) => (v*s ...)`
3. Then call `(num+ ...)` and wrap that into a `Result` value, so `(v*s (num+ ... ... ) ...)`

The result we get:

```scheme
(define (interp [a : ExprC] [env : Env] [sto : Store]) : Result
.......
    [plusC (l r)
           (type-case Result (interp l env sto) ; interp left expression
                      [v*s (v-l sto-l)  ; Value
                           ; Store
                           (type-case Result (interp r env sto-l) ; interp right expression with left store
                                      [v*s (v-r sto-r) ; obtain right value
                                           (v*s (num+ v-l v-r) (sto-r))] ; return latest state (sto-r) with the value)
                           ]
```





### The Store

Store is similar to rep. as Environment, but we can use a number as a location 



## Sweeter Syntax

- use `with`

```scheme
(define-syntax-rule
  (with [(v-id sto-id) call]
    body)
  (type-case Result call
    [v*s (v-id sto-id) body]))
```

define-syntax-rule: bind `with` to trigger  as syntatic sugar expansion (i.e. like a template)





# Variables

Motivation: ability to mutate variables (to work like boxes in that regard)



Variables and boxes: 

The point of having references is to update variables in place. 

### Example:

Variables and boxes -- boxes are like pointers in C (location with variable as identifier). Allowing mutations to be observabale -- can you 

- boxes/pointers
- every assignment can change the value

Go: assignment is a copy



tldr: boxes are not any more fundamental than variables.

RVCFAE: every function application is by reference.

