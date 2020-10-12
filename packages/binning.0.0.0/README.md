# Binning

A tiny library to count objects (for histograms) or sort them by
category (build an association or relation). Here is a typical use:

```ocaml
let histogram xs =
  List.to_seq xs
  |> Binning.counts
  |> List.of_seq
(* val histogram : 'a list -> ('a * int) list = <fun> *)
```

## Installation

```sh
opam install binning
```
