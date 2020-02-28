# ppx_distr_guards
OCaml ppx extension to distribute guards over or-patterns (Warning 57)

## Problem
~~~ocaml
type t = A of int | B of int
let f = function
  | A x, _ (* A 0, A 1 would match here and then fail the guard *)
  | _, A x when x<>0 -> 1 (* warning 57 *)
  | _ -> 2
(* sadly there's no easy way to have the guard checked for every pattern: *)
let f = function
  | A x, _ when x<>0 -> 1
  | _, A x when x<>0 -> 1 (* if these were big expressions, we would need to pull out a function for each case to avoid duplication *)
  | _ -> 2
~~~

    File "test.ml", line 28, characters 4-19:
    Warning 57: Ambiguous or-pattern variables under guard;
    variable x may match different arguments. (See manual section 8.5)

## Solution
Different possibilities (see `test.ml`). Decided on the following:
~~~ocaml
let f = function%distr (* applies to all cases, no brackets, does not compile w/o ppx *)
  | A x, _ | _, A x when x<>0 -> 1
  | _ -> 2
~~~
Which will result in
~~~ocaml
let f = function
  | A x, _ when x<>0 -> 1 | _, A x when x<>0 -> 1
  | _ -> 2
~~~
