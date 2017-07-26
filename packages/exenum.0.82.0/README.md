# ocaml-exenum

exenum is an **[OCaml](http://ocaml.org/) library** that helps building exhaustive or semi-exhaustive data sets, typically for intensive unit testing.

Inspired by [testing-feat](https://hackage.haskell.org/package/testing-feat) for Haskell.

## Install

Install with [opam](https://opam.ocaml.org/): `opam install exenum`

## API Documentation

The [Exenum API](https://lebotlan.github.io/ocaml-exenum/index.html).
See also the examples/ dir.

The ocamlfind package names are `exenum` and `exenum.lwt` (the latter is installed only if lwt is present).

When using exenum.lwt, you need to compile with the following packages (in ocamlbuild's _tag notation):
`package(lwt,lwt.unix,exenum,exenum.lwt,lwt.ppx)`


## Overview

We consider a simple example. 

* Assume that, for testing purposes, we need many values of type int list.
```ocaml
open Exenum

(* Build a new enumeration of type int list. *)
let enum_intlist = e_list e_int
```
* Because pretty-printing matters, we take the time to define string_of_intlist:
```ocaml
let string_of_intlist l = "[" ^ String.concat ", " (List.map string_of_int l) ^ "]"
```
* We ask Exenum to show a few values of this enumeration:
```ocaml
let () = show enum_intlist string_of_intlist 0 11
```
```
Value #0 is []
Value #1 is [2]
Value #2 is [0]
Value #3 is [3]
Value #4 is [1]
Value #5 is [-2]
Value #6 is [-1]
Value #7 is [-3]
Value #8 is [4, 2]
Value #9 is [1, 2]
Value #10 is [0, -2]
```
* We are curious. What are values at a very large index?
```ocaml
let index = Big_int.power_int_positive_int 10 200 (* Indeed, this is 10^200. *)
let () = bigshow enum_intlist string_of_intlist index 2
```
```
Value #0 is [-11141639911, 19603183504, -15283679472, 6656783909, 1634524031, -471077998, 112885229, -223859756, -132220646, -7807730, -47780089, 30956540, -11221453, 3859390, 2834978, 2087393, 895525, -306900, -121738, -9063, 29280, 23372, 13852, -6911, -2887, 1103, 846, -253, -68, -13, 43, 10, -4, -2]
Value #1 is [13635890183, -19174930941, -15838074444, -4484269954, -2648391618, -1149614834, 645802777, -18833984, 256671692, 42265941, -44644927, 19249984, 2972215, -5443265, 2028581, 844099, 519800, 375847, -76182, 27590, -64122, 29835, 14849, -7121, 4030, -327, -753, -498, 104, -8, 47, 0, -5, 4, -1]
```
* Notice how these two values, which are adjacent in the enumeration, are (purposely) significantly different.
* Computing values at a large index is quick (the complexity is basically logarithmic with respect to the index).

* Testing a function
```ocaml
(* This is the function we wish to test. *)
let mysum l = List.fold_left (+) 0 l

(* This is the test we perform on mysum. *)
let test_mysum l =
  let sum1 = mysum l
  
  (* Here, we use an oracle to check if the function is right. *)
  and sum2 = List.fold_right (+) l 0 in

  assert (sum1 = sum2)
  
(* Exenum provides a 'tester', which applies test_mysum continuously. *)  
let () = tester enum_intlist ~tos:string_of_intlist ~len:1000 ~verbose_period:10000 test_mysum
```
* The ~len argument is the number of consecutive indices which are tested before doubling the current index.
* A message is printed every ~verbose_period tests.

```
Test number 0, index #0 ... with [] ... done
Test number 10000, index #2046000 ... with [51, 8, -9, 3, 2] ... done
Test number 20000, index #2097150000 ... with [8, 0, 17, 3, 2, 2] ... done
Test number 30000, index #2147483646000 ... with [212, 107, -20, -9, 15, 0, -1] ... done
Test number 40000, index #2199023255550000 ... with [-158, 4, 30, -48, 15, 5, -7, -2] ... done
Test number 50000, index #2251799813685246000 ... with [47, 93, -244, -54, 5, 11, -13, 3, -3] ... done
Test number 60000, index #2305843009213693950000 ... with [746, -191, 499, 61, -58, 31, -21, 9, 5, 2] ... done
...
(and so on, you have to interrupt)
```

## Contact

Didier Le Botlan, **github.lebotlan@dfgh.met**  where you replace **.met** by **.net**.



