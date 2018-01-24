# minima.l

Minimal `lisp` interpreter in about 250 lines of OCaml code.
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

```lisp
# This is a comment
```

### Types

* List : `( ... )`
* Number: positive and negative 64-bit integer
* Symbol: character string
* String: `"`-delimited character string
* Function: internal function definition, inaccessible to the user

### Constants

* `NIL`: the empty list, also stands for `false`
* `_`: wildcard, used to disregard patterns during deconstruction
* `T`: stands for `true`

### Symbols

Symbols are used by the interpreter as keys to address `lisp` types in mappings
such as `def` or `setq`. They are also used to map function arguments in `def`
and in lambda definitions.

### Evaluation

* Numbers and strings evaluate to themselves
* Symbols evaluate to their values
* Lists evaluate as function call

When a symbol is evaluated, its value is returned. In the case of internal
functions (`car`, `cdr`, ...), the special `Function` type is returned and
displayed as `<fn>`:

```lisp
: car
-> <car>
```

### Lambda functions

The `lambda` keyword does not exist. It is replaced by list evaluation of the
form `'(args . prg)`. Arguments can be a single symbol or a list of symbols.
Example:

```lisp
: ('((X Y)(+ X Y)) 1 1)
-> 2

: ('(x (map '((n)(+ n 1)) x)) '(1 2 3 4))
-> (2 3 4 5)
```

_Ipso facto_, the following expressions are equivalent:

```lisp
(def add (X Y) (+ X Y))
(setq add '((X Y)(+ X Y)))
```

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
: (foldl '((acc (_ . v))(+ acc v)) 0 data)
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
(def sym args [str] prg)
```

Define a function with arguments `args` and body `prg` and associate it with
the symbol `sym`. An optional `str` can be specified as a documentation string
and is ignored by the interpreter.

```lisp
: (def add (X Y) (+ X Y))
-> add
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
: (let ((a 1)(b 2)) (println a b))
1
2
-> 2
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
    '(("inc" ((x)(+ x 2)))
      ("add" ((x y)(+ x y)))
      ("acc" ((l)(foldl + 10 l)))
      ))
-> (("inc" ((x) (+ x 2))) ("add" ((x y) (+ x y))) ("acc" ((l) (foldl + 10 l))))

: (def filter (lamdas)
    (foldr '(((_ fn) acc)
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
-> (((x) (+ x 2)) ((l) (foldl + 10 l)))

: (map '((l)(l 1)) (filter lambdas))
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
    (iter '((t)(prinl (join ", " t))) DATA))
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

## License

ISC. See `LICENSE.md`.
