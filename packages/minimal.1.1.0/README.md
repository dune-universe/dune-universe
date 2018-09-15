# minima.l

Minimal `lisp` interpreter in about 350 lines of OCaml code.
**Heavily** inspired by [Picolisp](https://picolisp.com).

## Example

```lisp
#
# Define the Fibonacci function
#
(def fib (N)
  (?: (<= N 1)
    N
    (+ (fib (- N 1)) (fib (- N 2)))
    ))

#
# Call the Fibonacci function
#
(prinl "Result: " (fib 30))
```

## Language

The encoding is expected to be UTF-8.

```lisp
# This is a comment
```

### Types

* List : `( ... )`
* Number: positive and negative 64-bit integer
* Symbol: character string
* String: `"`-delimited character string
* Function: the function type as defined by `def` or `λ`
* Internal: internal function definition, inaccessible to the user

### Constants

* `T`: stands for `true`
* `NIL`: the empty list, also stands for `false`
* `_`: wildcard, used to disregard patterns during deconstruction

### Symbols

The interpreter uses symbols to address `lisp` types. Symbols can be created or
altered using the `def` and `setq` functions. Internals are pre-mapped into the
symbol world and can be overridden by the user.

### Evaluation

* Numbers and strings evaluate to themselves
* Symbols evaluate to their values
* Lists evaluate as function call

When a symbol is evaluated, its value is returned. In the case of internal
functions (`car`, `cdr`, ...), the special `Internal` type is returned and
displayed as `<internal>`:

```lisp
: car
-> <car>
```

### Lambda functions

Lambda functions are defined using the `λ` keyword (codepoint 0x3BB).
Invocation of `λ` is similar to `def`:

```lisp
: ((λ (X Y)(+ X Y)) 1 1)
-> 2

: ((λ x (map (λ (n)(+ n 1)) x)) '(1 2 3 4))
-> (2 3 4 5)
```

### Closures

Function definitions carry a symbol closure. The closure contains the value of
the function's symbols *not present in the argument list* such as they were
resolved at the definition site. 

### Currying

Function can be curried:

```lisp
: ((λ (a b) (+ a b)) 1)
-> (λ (b) (+ a b))
: (@ 10)
-> 11
```

Curryring is available for all user-defined functions as well as for some internal
functions such as `+`, `-`, `and`, `cons`, and so on. Many shortcuts can
therefore been defined as such:

```lisp
: (setq =0 (= 0))
-> (λ (a) (= 0 a))
: (=0 0)
-> T
: (=0 1)
-> Nil
```

### Recursion

Function defined using `def` can be recursive, i.e. call themselves. When
functions are defined, symbols with their name are not resolved in their closure
and are resolved in the symbol domain instead. Same goes when multiple
functions are defined using `def`, allowing mutual recursion between functions.
However, there is a caveat: since the function symbols are resolved dynamically,
redefining these symbols will lead to undefined behavior.

### Argument assignation

Assignation of arguments in either `def` of `lamda` functions support
deconstruction. For instance, with `def`:

```lisp
: (def sum3 ((a b c)) (+ (+ a b) c))
-> sum3
: (sum3 (list 1 2 3))
-> 6
```

Or with a lambda:

```lisp
: (setq data '(("hello" . 1) ("world" . 2)))
-> (("hello" . 1) ("world" . 2))
: (foldl (λ (acc (_ . v))(+ acc v)) 0 data)
-> 3
```

## Functions

> A quoted (') argument means that it is evaluated by the function.

### Basics

##### args

```
(args)
```

When executed in a script, return the list of command line arguments passed to
the script.

```lisp
#!/usr/bin/env mnml

(println args)
```

##### def

```lisp
(def sym args [str] prg ...)
```

Define a function with arguments `args` and body `prg` and associate it with
the symbol `sym`. An optional `str` can be specified as a documentation string
and is ignored by the interpreter.

```lisp
: (def add (x y) (+ x y))
-> add
```

Function defined with the `def` keyword are simply lambda functions assigned to
symbol. Indeed, the following expression are strictly equivalent:

```lisp
: (def add (a b) (+ a b))
-> add
: (setq add (λ (a b) (+ a b)))
-> (λ (a b) (+ a b))
```

Multiple functions can be defined at the same time. This is especially useful
when functions are mutually recursive:

```lisp
: (def
    a0 (n) (?: (< n 10) (b0 (+ n 1)) (cons 'a0 n))
    b0 (n) (?: (< n 10) (a0 (+ n 1)) (cons 'b0 n)))
-> b
: (a0 1)
-> (b0 . 10)
```

##### eval

```lisp
(eval any)
```

Evaluate `any`.

```lisp
: (eval '(+ 1 1))
-> 2
```

##### let

```lisp
(let lst . prg)
```

Evaluate `prg` within the context of the bind list `lst`. The bind list has the
following format:

```lisp
((any . 'any)(any . 'any)...)
```

For each element in the bind list, the `cdr` is evaluated and bound to its `car`
using the argument assignation process described above.

```lisp
: (let ((a . 1)(b . 2)) (println a b))
1
2
-> 2
```

##### lift

```lisp
(lift 'fn)
```

Lift a function object into a lambda definition list. The function's closure is
lost in the process.

```lisp
: (def add (a b) (+ a b))
-> add
: add
-> λ ((a b) (+ a b))
: (lift @)
-> (λ (a b) (+ a b))
```

##### load

```lisp
(load . str)
```

Load the `lisp` file pointed by `str`. On success, `load` returns the result of
the last evaluated operation in the file. Otherwise, `NIL` is returned.

```lisp
: (load "examples/fibonacci.l")
-> 832040
```

If the path is prefixed by `@lib`, `load` will look for the file in the library
directory of the installation prefix.

```lisp
: (load "@lib/list.l")
-> assoc
```

##### quote

```lisp
(quote . any)
```

Quote `any`. The form `'any` is a syntactic shortcut for this function.

```lisp
: (quote . a)
-> a
```

##### setq

``` lisp
(setq sym 'any [sym 'any] ...)
```

Associate `any` with the symbol `sym`. Multiple associations can take place at
once.

```lisp
: (setq A (+ 1 2) B (* A 2))
-> 6
: A
-> 3
: B
-> 6
```

##### sym

```
(sym . str)
```

Make a symbol of `str`.

```lisp
: (sym . "+")
-> +
: (eval (sym . "+"))
-> <+>
: ((sym . "+") 1 1)
-> 2
```

### List processing

##### car

```lisp
(car 'lst)
```

Return the head of a list.

```lisp
: (car (1 2 3 4))
-> 1
```

##### cdr

```lisp
(cdr 'lst)
```

Return the tail of a list.

```lisp
: (cdr (1 2 3 4))
-> (2 3 4)
```

##### conc

```lisp
(conc 'any ...)
```

Concatenate multiple lists into one.

##### cons

```lisp
(cons 'any ...)
```

Construct a new list cell using the first argument for `car` and the remaining
arguments for `cdr`.

```lisp
: (cons 1 2)
-> (1 . 2)
: (cons 1 2 3)
-> (1 2 . 3)
: (cons 1 (cons 2 3))
-> (1 2 . 3)
```

##### list

```lisp
(list 'any ...)
```

Create a list with `any` arguments.

```lisp
: (list)
-> (NIL)
: (list (+ 1 1) 3 "a")
-> (2 3 "a")
: (list (setq A 1) 2 (+ A 2))
-> (1 2 3)
```

### Predicates

```lisp
(nil? 'any)
(num? 'any)
(str? 'any)
(sym? 'any)
(lst? 'any)
(fun? 'any)
```

Respectively test if `any` is a number, a string, a symbol, a list, or an
internal function. Return `T` on success, `NIL` otherwise.

### Logic

```lisp
(and 'any 'any)
(or 'any 'any)
(not 'any)
```

Perform the respective logic evaluation. In each case, `any` must evaluate to
`T` or `NIL`.

### Flow control

##### ?

```lisp
(? 'any prg)
```

When `any` evaluates to `T`, return the evaluation of `prg`. Return `NIL`
otherwise.

```list
: (def test (v) (? (> v 10) (* v 2)))
-> test
: (test 5)
-> NIL 
: (test 20)
-> 40
```

##### ?!

```lisp
(?! 'any prg)
```

When `any` evaluates to `NIL`, return the evaluation of `prg`. Return `NIL`
otherwise.

```list
: (def test (v) (?! (> v 10) (* v 2)))
-> test
: (test 5)
-> 10 
: (test 20)
-> NIL
```

##### case

```lisp
(case 'any (any . prg) (any . prg) ...)
```

Compare `any` with the `car` of the remaining arguments and return the
evaluation of the first positive match. The _default_ or _catch all_ case is
written using the special value `_` as `car`.

Order is important. If multiple match exist, the first one is evaluated. If `_`
is placed before a valid match, `_` is evaluated.

```lisp
: (def test (v) (case v ("hello" . "world") ("foo" . "bar") (_ . "unknown")))
-> test
: (test "hello")
-> "world"
: (test "foo")
-> "bar"
: (test "bonjour")
-> "unknown"
```

Also, using the wildcard symbol `_`, structural matches can be performed. For
instance, here is an example that filters a list of lambdas based on their
arity:

```lisp
: (setq lambdas
    '(("inc" `(λ (x) (+ x 2)))
      ("add" `(λ (x y) (+ x y)))
      ("acc" `(λ (l) (foldl + 10 l)))
      ))
-> (("inc" ((x) (+ x 2))) ("add" ((x y) (+ x y))) ("acc" ((l) (foldl + 10 l))))

: (def filter (lamdas)
    (foldr (λ ((_ fn) acc)
             (case fn
               (((_  ) _) (cons @ acc))
               (((_ _) _) acc)
               (_         acc)
               ))
           lambdas
           ()
           ))
-> filter

: (filter lambdas)
-> ([(x) (+ x 2)] [(l) (foldl + 10 l)])

: (map (λ (l)(l 1)) (filter lambdas))
-> (3 11)
```

##### ?:

```lisp
(?: 'any prg1 prg2)
```

If `any` evaluates to `T`, return the evaluation of `prg1`. Return the
evaluation of `prg2` otherwise.

```lisp
: (def test (v) (?: (> v 10) (* v 2) (* v 3)))
-> test
: (test 5)
-> 15
: (test 15)
-> 30
```

##### prog

```lisp
(prog prg1 prg2 ...)
```

Evaluate `prg1`, `prg2`, ..., in sequence and return the last evaluation.

```lisp
: (prog (+ 1 1) (+ 2 2))
-> 4
```

##### while

```lisp
(while 'any prg)
```

Execute `prg` while `any` is not `NIL`.

```lisp
: (while (read) (prinl @))
(+ 1 1)
<+>11
("hello" "world")
helloworld
NIL
-> ("hello" "world")
```

### Exceptions

##### catch

```lisp
(catch prg (any . 'any) (any . 'any) ...)
```

Catch exceptions thrown in `prg`. The exception is compared to  the `car` of
the catch clause. In case of a match, the `cdr` of the clause is run.

```lisp
: (catch
    (throw "hello")
    ("hello" . "world")
    ("foo" . (println "bar")))
-> "world"
```

##### throw

```lisp
(throw 'any)
```

Trow an exception with argument `any`.

### Comparisons

##### =

```lisp
(= 'num 'num)
(= 'str 'str)
(= 'lst 'lst)
```

Structural equality.

##### <>

```lisp
(<> 'num 'num)
(<> 'str 'str)
(<> 'lst 'lst)
```

Structural inequality. Inverse of `=`.

##### ge, gt, le, lt

```lisp
(ge 'num 'num)
(gt 'num 'num)
(le 'num 'num)
(lt 'num 'num)
```

### Arithmetics

```lisp
(add 'num 'num)
(div 'num 'num)
(mul 'num 'num)
(sub 'num 'num)
```

### String operations

##### join

```lisp
(join 'sep 'lst)
```

Join the list of strings `lst` into a single string using `sep` as a delimiter.
When `sep` evaluates to `NIL` instead of a string, no separator is used.

```lisp
: (join ":" ("a" "b" "c"))
-> "a:b:c"
```

##### split

```lisp
(split 'sep 'str)
```

Split `str` into multiple strings using `sep` as a delimiter. When `sep`
evaluates to `NIL` instead of a string, individual characters are returned.

```lisp
: (split " " "hello world")
-> ("hello" "world")
```

### I/O

##### flush

```lisp
(flush)
```

Flush the current output channel.

##### json

```lisp
(json 'sym 'str)
```

Parse JSON input. `sym` can either be `file` or `string`. This function piggies
back on Yojson.

```lisp
: (json 'string "{ \"hello\": \"world\" }")
-> (("hello" . "world"))
```

##### in

```lisp
(in 'any . prg)
```

Create a new input channel context and evaluate `prg` within that context. The
previous context is restored after the evaluation. When the first argument
evaluates to `NIL`, the context uses `stdin`. When the argument evaluates to a
string, `in` assumes the string contains a file path and tries to open that
file.

```lisp
: (in NIL (read))
1
-> 1
```

##### out

```lisp
(out 'any . prg)
```

Create a new output channel context and evaluate `prg` within that context. The
previous context is restored after the evaluation. When the first argument
evaluates to `NIL`, the context uses `stdout`. When the argument evaluates to a
string, `out` assumes the string contains a file path and tries to open that
file.

If the file does not exist, it is created. If the file exists, it is truncated.
If the file path is prepended with a `+` the file must exist and data will be
appended to it.

```lisp
: (setq DATA '(("Name"   "Age" "City"  )
               ("Alex"   "32" "London" )
               ("John"   "17" "Chicago")
               ("Marc"   "25" "Lyon"   )
               ("Sophie" "29" "Nice"   )
               ))
-> (("Name" "Age" "City") ("Alex" "32" "London") ("John" "17" "Chicago") ("Marc" "25" "Lyon") ("Sophie" "29" "Nice"))

: (out "result.csv"
    (iter (λ (t)(prinl (join ", " t))) DATA))
-> ("Sophie" "29" "Nice")
```

```bash
$ cat result.csv
Name, Age, City
Alex, 32, London
John, 17, Chicago
Marc, 25, Lyon
Sophie, 29, Nice
```

##### prin

```lisp
(prin 'any ...)
```

Print the string representation of `any`. When multiple arguments are printed,
no separator is used. The last argument is returned after evaluation.

```lisp
: (prin "hello, " "world!")
hello, world!-> "world!"
```

##### prinl

```lisp
(prinl 'any ...)
```

Calls `prin` and appends a new line.

```lisp
: (prinl "hello, " "world!")
hello, world!
-> "world!"
```

##### print

```lisp
(print 'any ...)
```

Print the lisp representation of `any`. When multiple arguments are printed, a
space separator is used. The last argument is returned after evaluation.

```lisp
: (print 'a 'b (1 2 3) +)
a b (1 2 3) <+>-> <+>
```

##### println

```lisp
(println 'any ...)
```

Calls `print` and appends a new line.

```lisp
: (print 'a 'b (1 2 3) +)
a b (1 2 3) <+>
-> <+>
```

##### read

```lisp
(read)
```

Read one lisp token from the current input channel.

```lisp
: (read)
(1 2 3)
-> (1 2 3)
```

### System

##### env

```list
(env 'any ['any'])
```

If `any` evaluates as a string, return the value of the environment variable
with that string. If a second `any` argument is provided and evaluates as a
string, set the value of the environment variable to that string and return the
previous value.

```lisp
: (env "PATH")
-> "/usr/bin:/bin"
: (env "PATH" (join ":" (list "/usr/local/bin" (env "PATH"))))
-> "/usr/bin:/bin"
: (env "PATH")
-> "/usr/local/bin:/usr/bin:/bin"
```

##### quit

```lisp
(quit)
```

Terminate the top-level evaluation.

### Debug

##### closure

Print the closure of a function definition.


```lisp
: (def add (a b) (+ a b))
-> add
: (setq +1 (add 1))
-> (add (b) (+ a b))
: (closure +1)
-> (("a" . 1) ("+" . <+>))
```

##### trace

Toggle tracing.

## Appreciation

Tokens of appreciation are gladly accepted in the form of [virtual coffee](https://buymeacoff.ee/xguerin).

## License

ISC. See `LICENSE.md`.
