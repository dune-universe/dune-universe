Results for Computability Theory
================================


Language Definition
-------------------


Definition:

>   A language `L = (P, eval)` consists of a set `P` of programs where `P` is a
>   subset of the natural numbers and an evaluation function

>       eval: P x N -> N

>   which is a partial function. It is decidable, if a natural number is a
    program, i.e. there is a total computable function `valid: N -> {0,1}` where `valid
    (n) = 1` means that `n` is a valid program.

We define programs in the domain of natural numbers. This is not a restriction,
because every file or collection of files can be interpreted as a natural
number, because they are sequence of bytes and every sequence of bytes can be
mapped one-to-one to a natural number.

Furthermore we consider only functions from natural numbers to natural numbers.
This is not a restriction either for the same argument.

Note that partial functions are functional relations, i.e. `r n m1` and
`r n m2` imply `m1 = m2` when `r` is a functional relation. A relation is left
total, if for all `x` in its domain there is a `y` such that `r x y`. Therefore
a functional, left-total relation represents a total function.

Definition:

>   A language is total, if its evaluation function is total.




Implementation of Functions
---------------------------

Definition:

>   Assume a language `L = (P,eval)`. We say that an `F in P` implements a
>   function `f: N -> N`, if

>       eval(F, m) = f(m)

>   for all `m in N`.


Definition:

> A language `L = (P, eval)` is closed under composition if for all functions
> `f` and `g` which it implements, it implements the composition of the
> functions `f` and `g` as well.





Interpretation of a Language
----------------------------

Definition:

>   Assume two languages `L1 = (P1, eval1)` and `L2 = (P2, eval2)`. We say that
>   `L1` *interprets* `L2` if there is an interpreter program `u` in `P1` such
>   that

>       eval1(u, <n,m>) ~ eval2(n, m)

>   for all valid programs `n` in `P2` and `m` in `N`.


In this definition we use a pairing function `<n,m>` which maps each pair of
natural numbers one-to-one to a natural number.

Furthermore we don't use equality in this definition. We use equivalence. I.e.
if `eval1(u, <n,m>)` is undefined, then `eval2(n, m)` is undefined as well and
vice versa. If both functions are defined for the arguments, then they
return the same value.


A language `L = (P, eval)` interprets itself, if it has an interpreter program
`u` such that

    eval(u, <n,m>) ~ eval(n, m)

for all valid programs `n`.





Total Languages cannot interpret themselves
-------------------------------------------


In the following we prove the fact that a total language cannot interpret
itself as long as it implements at least the successor function and the diagonal
function and is closed under composition.

Assume that `L = (P, eval)` is a total language which has an interpreter program
`u` which implements self interpretation.

Therefore we get for all valid programs `n` and all natural numbers `m`

    eval(u, <n,m>) = eval(n,m)

Here we have equality, because being a total language the evaluation function is
total.

According to our definitions, the program `u` implements the function

    x |-> eval(u, x)

because of the triviality

    eval (u, x) = eval(u, x)

Furthermore we have the programs `succ` and `diag` such that

    eval(succ, m) = m + 1

    eval(diag, m) = <m,m>

Now we define a function `f: N -> N` as

    f(k) := 1 + eval(u, <k,k>)

In that definition it is crucial, that the language is total. Therefore `eval`
is total and the above definition is a valid function definition. If the
language were not total, the above definition of `f` would be illegal.

Because the language is closed under composition, there is a program `F` such
that

    eval(F, k) = f(k)

for all natural numbers `k`.

Now we form `eval(u, <F,F>)` and get

    eval(u, <F,F>)

    = eval(F, F)            -- `u` is interpreter program

    = f(F)                  -- `F` implements `f`

    = 1 + eval(u, <F,F>)    -- definition of `f`

This is a contradiction because a number and its successor cannot be equal.
Therefore our assumptions are wrong and either the language `L` is not total or
the language does not interpret itself.







Compiling
=========

Machine Language
----------------

We assume that we have a machine language
```
    LM = (PM, evalM)
```

whose programs are numbers representing a programs in machine language which can
implement one argument functions on natural numbers.

Cleary the machine language is not total. I.e. there are machine programs `n`
such that `evalM(n, m)` is not defined for all `m`.

The machine is basically a computer which executes a machine program given as a
file which we represent here by a natural numbers on a certain input file, which
we represent by an natural number as well, and outputs another file which we
represent by a natural number as well.




Language Definition with a Compiler
-----------------------------------

Definition:

>   A compiler from language `A = (PA, evalA)` to language `B = (PB, evalB)`
>   written in language `C = (PC, evalC)` is a program `C_ABC` from `PC` which
>   takes as input a program `S_A` from `PA` and produces a program `T_A` from
>   `PB` as output.

>   If `A`, `B` and `C` are given, the following should hold for all natural
>   numbers `m` and all valid programs `S_A`:

>       evalA(S_A, m) ~ evalB(evalC(C_ABC, S_A), m)

If we want the compiler `C_ABC` to implement a total function, we can slightly
modify the previous definition and require that `C_ABC` implements the function
`f_ABC` with

```
                   /  <1, T_A>,   if S_A in PA
    f_ABC := p -> |
                   \  <0,0>,      otherwise
```

Since `PA` is decidable, `f_ABC` is total and hence `evalC(C_ABC, p)` is defined
for all natural numbers `p` and we require

```
    evalA(S_A, m) ~ evalB(second(evalC(C_ABC, S_A)), m)
```
to hold for all natural numbers `m` and all valid programs `S_A`.

We can also **define** a language `A = (PA, evalA)` by a compiler `C_ABC`. The
compiler takes as input a natural number `p` and produces a program `T_A` in
target language `B` (e.i. an element of `PB`) or outputs an error.

The compiler `C_ABC` distinguishes success and error by implementing a total function

```
    f_ABC: N -> <1,PB> V <0,0>
```

We can now define the language `A=(PA, evalA)` as follows:

```
    PA := { n in N | first(evalC(C_ABC, n)) = 1 }
```
Since `f_ABC` is total, `PA` is decidable.

```
    evalA(S_A, m) :=
        evalB(
            second(evalC(C_ABC, S_A)),
            m
        )
```

This completes the definition of the language `A = (PA, evalA)`.

Language `A` is total if its evaluation function `evalA` is total. Since we did
not require `B` to be a total language, `evalA` is total iff `evalC(C_ABC, S_A)`
does only produce target code `T_A` in `PB` which implements a total function.




Implement a Compiler in its own Language
----------------------------------------

What does it mean to implement a compiler of a high level language in its own
language?

In this case the source language and the implementing language are the same, so
we call such a compiler `C_ABA`. `C_ABA` is a compiler from source language `A`
to target language `B` written in the source language `A`, i.e. `C_ABA` is an
element of `PA`.

Let the language `A = (PA, evalA)` be given. According to our definition `C_ABA`
defines a valid compiler if

    evalA(S_A, n) ~ evalB(second(evalA(C_ABA, S_A)), n)

for all natural numbers `n` and valid programs `S_A`. Furthermore, `evalA(C_ABA,
    S_A)` should evaluate to `<0,0>` if `S_A` is not an element of `PA`.

Let the language `A = (PA, evalA)` be defined by the compiler `C_ABC`.
If `C_ABA` really implements a compiler of `A`, then the following equivalence
must be valid

```
    evalA(C_ABA, S_A) ~  evalC(C_ABC, S_A)
```

I.e. both `C_ABA` and `C_ABC` must compile the same source code `S_A` into the
same target code `T_A` if `S_A` is a valid `A` program or both fail on the
input, if `S_A` is not a valid `A` program.

This is a very strong requirement. Instead of requiring the target code `T_A`
to be the same for both compilers, it would probably be sufficient to require that
the target codes **behaves** the same, i.e. for every input `m` they return the
same output:

```
    evalB(second(evalA(C_ABA, S_A)), m) ~ evalB(second(evalC(C_ABC, S_A)), m)
```


Open Question
-------------

Given a compiler `C_AA` written in its own language. Is it possible to write an
interpreter `u` in `PA` which interprets `LA`?

If the answer to this question is yes, then a compiler for a total language
cannot be written in its own language.

More general: Assuming that `C_AA` exists and `LA` is total. Does this lead to a
contradiction?

Example of a Total Language with a Self-Compiler
-----------------------------------------------

Suppose we hava a language

    A = (PA, evalA)

with the following properties:

* `A` is a total language, i.e. `evalA` is total.
* `A` implements the function c

```
                  / <1,x>,    if x in PA
      c :=  x -> |
                  \ <0,0>,    otherwise
```
This means, `A` is a total language being able to implement its own
parser/typechecker.

Let's call the program implementing the function `c` `C_AAA`. `C_AAA` defines a
valid compiler from language `A` to language `A` written in language `A`, since

```
evalA(second(evalA(C_AAA, S_A)), n)
    = evalA(second(<1,S_A>), n)
    = evalA(S_A, n)

```
for all natural numbers `n` and valid programs `S_A` and

    evalA(C_AAA, S_A) = <0,0>

if `S_A` is not a valid program.

This shows that `A` is a total language with a (admittedly very "esoteric")
self-compiler.

But we can go one step further.


Suppose we have language `A` defined as above and language `B = (PB, evalB)`
with the following additional properties:

* `PA` is a subset of `PB`
* `evalA(p,n) = evalB(p,n)` for all natural numbers `n` and valid programs `p`
  of `A` (i.e. `p` is element of `PA`).

This means, language `B` is an extension of language `A` (and does not need to
be total - we don't pose any restrictions on the extension as long as `evalB`
behaves the same as `evalA` for all programs from language `A`).

Since language `A` is a subset of language `B`, writing a compiler from `B` to
`A` boils down to writing a decision procedure for `PA` (i.e. a
parser/typechecker). The actual compiler consists of the identity function.

With these requirements, the program `C_ABA := C_AAA` defines a valid compiler
from language `A` to language `B` written in language `A`, since

* For all natural numbers `n` and valid source programs `S_A`, the following
holds:

```
    evalB(second(evalA(S_ABA, S_A)), n)
        = evalB(S_A, n)
        = evalA(S_A, n)
```

* `evalA(S_ABA, S_A) = <0,0>` if `S_A` is not an element of `PA`.

These examples show that any total language, which is able to implement its own
decision procedure (i.e. distinguish valid programs from invalid ones) is able
to express some form of self-compiler. The examples don't touch the source code
(and hence don't implement any source code tranformations), since the actual
compilation consists of the identity function. But according to our definition,
they are valid self-compilers.

It remains to be explored if there are any restrictions to implementing a
self-compiler compiling into a target language different from the source
language (i.e. implementing "real" code transformations)These examples show that
any total language, which is able to implement its own decision procedure (i.e.
distinguish valid programs from invalid ones) is able to express some form of
self-compiler. The examples don't touch the source code (and hence don't
implement any source code tranformations), since the actual compilation consists
of the identity function. But according to our definition, they are valid
self-compilers.

It remains to be explored if there are any restrictions to implementing a
self-compiler compiling into a target language different from the source
language (i.e. implementing "real" code transformations). But at least there
doesn't seem to be an inherent problem of expressing self-compilers in a total
language.
