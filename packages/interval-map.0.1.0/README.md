# interval-map

[![Actions Status](https://github.com/dgllghr/interval-map/workflows/CI/badge.svg)](https://github.com/dgllghr/interval-map/actions)

An immutable interval map data structure implemented as an interval tree. Based on [jgblight/im_interval_tree](https://github.com/jgblight/im_interval_tree).

Interval maps are great for finding intervals and their associated values which overlap a given interval. This interval map supports intervals with excluded, included, and unbounded bound ends. Multiple values may be associated with the same interval.

## Installation

```bash
opam install interval-map
```

## Usage

### In OCaml

```ocaml
let module Ivl_map = Interval_map.Make (Int) in
let module Ivl = Ivl_map.Interval in
(* Build the map *)
let map =
  Ivl_map.empty
  |> Ivl_map.add (Ivl.create (Included 0) (Excluded 10)) "foo"
  |> Ivl_map.add (Ivl.create (Included 0) (Excluded 10)) "foo2"
  |> Ivl_map.add (Ivl.create (Excluded 0) (Included 10)) "bar"
  |> Ivl_map.add (Ivl.create (Included 5) (Included 10)) "baz"
  |> Ivl_map.add (Ivl.create (Excluded 4) (Excluded 10)) "oof"
  |> Ivl_map.add (Ivl.create Unbounded (Excluded 4)) "zab"
in

(* Query the map *)
let query = Ivl.create Unbounded (Included 4) in
Ivl_map.query_interval query map
|> Ivl_map.Query_results.to_list
(* Results in:
  [({Ivl_map.Interval.low = Ivl_map.Bound.Unbounded; high = Ivl_map.Bound.Excluded 4},
    ["zab"]);
    ({Ivl_map.Interval.low = Ivl_map.Bound.Included 0; high = Ivl_map.Bound.Excluded 10},
    ["foo2", "foo"]);
    ({Ivl_map.Interval.low = Ivl_map.Bound.Excluded 0; high = Ivl_map.Bound.Included 10},
    ["bar"])]
  *)
```
