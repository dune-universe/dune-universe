# OSnap
Random snapshot testing library for OCaml.

The Documentation can be found [here](https://vch9.github.io/osnap/). This library
uses [QCheck](https://github.com/c-cube/qcheck) generators to generate random values
for snapshots.

## Build
Build from source:
```
$ git clone git@github.com:vch9/osnap.git
$ dune build
```
You can also install with opam:
```
$ opam install osnap
```

## License
The code is now released under the MIT license.

## An introduction to the library

The library intends to test differences on results between two versions.

### Binary Exponentiation
Let's say we write a function computing the exponentiation.
```ocaml
let rec exponentiation x n =
  if n = 0 then 1
  else if n = 1 then x
  else exponentiation x (n - 1) * x
  
let test =
  let open Osnap in
  let spec = Spec.(small_int ^> small_int ^>> string_of_int) in
  let path = ".osnap/exponentiation" in
  
  let test = Test.make ~spec ~path ~count:5 ~name:"exponentiation" exponentiation
  
let _ =
 Osnap.Runner.(run_tests ~mode:Interactive [ test ])
```
In the above, we first provide a naive implementation of the exponentiation.
Then, we create a test with the according specification using `Osnap` combinator.

```
exponentiation 14	4	38416
exponentiation 13	11	1792160394037
exponentiation 11	1	11
exponentiation 12	2	144
exponentiation 2	14	16384
```

As we agree with our function result, we promote the change by typing `Y`.
We try to improve our function efficiency with binary exponentiation.

```ocaml
let rec binary_expo x n =
  if n = 0 then 1
  else if n mod 2 = 0 then
    let tmp = binary_expo x (n / 2) in
    tmp * tmp
  else x * binary_expo x (n - 1)
  
let test =
  let open Osnap in
  let spec = Spec.(small_int ^> small_int ^>> string_of_int) in
  let path = ".osnap/exponentiation" in

  Test.make ~spec ~path ~count:5 ~name:"exponentiation" binary_expo

let _ = Osnap.Runner.(run_tests ~mode:Interactive [ test ])
```

Finally, we get the following result:
```
Recap:
      1 passed
      0 promoted
      0 ignored
      0 fails
```
`Osnap` has not found any difference between the old and new snapshot :heavy_check_mark:.
