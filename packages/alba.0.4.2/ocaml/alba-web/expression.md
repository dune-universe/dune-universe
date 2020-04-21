# Alba Expressions


## Simple Expressions


Up to now we have only the primitive types `Int`, `String`, `Character` and a
very limited set of operations on it.

    1 + 2 * 8

    "Hello " + "world"

    (+) 1 2

    Int -> String

    'A' = 'B'


All valid expressions have a type. The expression `1` has the type `Int`, the
expression `(+)` has the possible types `Int -> Int -> Int` and `String ->
`String -> String`.



## Function Application

Like in all functional languages, function application is by juxtaposition. I.e.
instead of

    f (x,y)

we write

    f x y

in order to apply the function `f` to the arguments `x` and `y`.

Infix notation `a + b` is used for operator expressions. But it is perfectly
possible to use an operator as a normal function, just put parentheses around
the operator. The following two expressions are equivalent.

    "Hello " + "world!"

    (+) "Hello " "world!"

A function can be partially applied. If `f` is a function with 2 arguments i.e.
its type is something like `A -> B -> C`, we can apply `f` only to the first
argument `f a` which returns not a value but a function of type `B -> C`. E.g.

    (+) "Hello" : String -> String

Operators have lower precedence than function application.

    f a b + g c

is parsed as

    (f a b) + (g c)

Therefore a function argument must be put in parentheses if it is an operator
expression

    f (a + b) (c - d)

Furthermore function application is left associative.

    f a b c ...

is parsed as

    ((f a) b) c

In order to save some parentheses and to write some expressions more intuitive,
Alba has the left associative operator `|>` and the right associative operator
`<|`. The following expressions on the left and right are equivalent.

    g (f a)                                 a |> f |> g

    f (a + b)                               a + b |> f

    f (a + b)                               f <| a + b

    f a b (g (c + d))                       f a b <| g <| c + d




## Function Expressions

The term

    \x y := defining_expression

represents a function with 2 arguments `x` and `y`. The arguments usually occur
in the defining expression `exp`. Example

    \ x y := "Hello "  + x + "and " + y

This expression has type

    String -> String -> String

A function expression can be applied to actual arguments

    (\ x y := "Hello "  + x + " and " + y) "Jim" "Joe"

This expression evaluates to

    "Hello Jim and Joe"

A function can be partially applied, returning a function which can be applied
to the missing argument. I.e.

    (\ x y := "Hello " + x + " and " + y) "Jim"

has type

    String -> String


In many cases the compiler can infer the types. But it is possible to add type
annotations

    \ (x: String) (y: String): String :=
        "Hello " ...




## Local Definitions

Expressions can use local definitions.

    1 + a where
        a := 25

    1 + f 10 where
        f x := 2 * x

    1 + f 5 where
        f x := 1 + g x
        g x := 2 * x

Local definitions can be annotated with types

    1 + f 10 where
        f (x: Int): Int := 2 * x







## Types

In a strongly type language all terms have types. In Alba types have types as
well. All simple types like `Int`, `String` and `Character` have type `Any`.
Therefore it is quite natural to have type valued functions like

    List: Any -> Any

I.e. `List` is a function which can be applied.

    List String: Any

Since types have a type, it is possible to construct type value functions.

    \ A := List (List A)

or with the long form

    \ (A: Any): Any := List (List A)




## Dependent Types

Usually functions have types like

    Int -> String

mapping an argument of type `Int` to a result of type `String`.

In addition to these *normal* function types we have dependent types where the
result type can depend on the argument. The general form of a dependent type is

    all (x: A): B

where `x` can occur in `B`. The type expression

    A -> B

is just a shorthand for

    all (x: A): B

where the argument `x` does not occur in the result type `B`.

The expression

    all (x: A) (y: B) ... : R

is a shorthand for

    all (x: A): all (y: B): ... : R


Dependent types are needed to assign types to polymorphic functions. The
operator `|>` has the type

    (|>):  all (A: Any) (a: A) (B: Any) (f: A -> B): B

I.e. under the hood `|>` is a function with 4 arguments. But how can a binary
operator have 4 arguments?

The solution is quite simple. The arguments `A` and `B` are type arguments which
occur in the subsequent argument argument or result types. Therefore the
compiler treats them as *implicit* arguments and infers them from the actual
arguments or the required result type.

E.g. the expression `(|>) "Hello"` has type

    (|>) "Hello" : all (B: Any) (f: String -> B): B

Looking at `(|>) "Hello"` the compiler infers `String` as the value of the first
implicit argument `A`. It is not yet able to infer the value of `B`.

However writing

    (|>) "world" ((+) "Hello ")

gives the compiler enough information to infer the type of the implicit argument
`B`.

Can you imagine what the type of `(<|)` is?




## Propositions

Propositions are assertions which exist only in the source code. E.g.

    "Hello" = "world"

is the proposition stating that the two strings are equal. Evidently this
assertion is false. Propositions are types. An expression of type `"Hello" =
"world"` is an evidence that the proposition is valid (i.e. it is a *proof* of
the proposition).
