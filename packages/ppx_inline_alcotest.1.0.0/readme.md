# PPX Inline Alcotest

Inline test backend for Alcotest.

## Setup

Add inline tests to your dune file, and preprocess using the test runner:
```dune
(library
 (name example)
 (inline_tests)
 (preprocess (pps ppx_inline_alcotest)))
```

Then, in OCaml, define tests using the same syntax as `ppx_inline_test`:
```ocaml
let double a = a + a

let%test "doubles argument" = Alcotest.(check int) "same int" (double 3) 6 
let%test "doubles argument" = Alcotest.(check int) "same int" (double 4) 8 
```

Then, to run the inline tests, simply run `dune build runtest`, and
this will call out to Alcotest, grouping tests by the files.

```
Testing `example'.
This run has ID `BA802DB5-953C-47A7-ACF2-5783B9B33163'.

  [OK]          example/example.ml          0   doubles argument.
  [OK]          example/example.ml          1   doubles argument.
```

## Dependencies

- dune >= "2.0"
- ppxlib >= "0.22.0"
- alcotest >= "1.0.0"
