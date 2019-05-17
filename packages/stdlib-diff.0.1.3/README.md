# Symmetric Diffs for OCaml Standard Library

ReasonML implementation of fast symmetric diffs for Sets and Maps. Library is designed to be used together with OCaml standard library that is shipped with [BuckleScript](https://bucklescript.github.io/).

Algorithm was inspired by Jane Street's [Base](https://github.com/janestreet/base) library _(that unfortunately cannot be used in BuckleScript directly)_.

## Installation

Install the package using `npm`:

```
npm install ocaml-diff
```

Add dependency to your `bsconfig.json`:

```
"bs-dependencies": [
  "ocaml-diff"
],
```

## Usage

For more examples see `examples` folder.

**Note:** Diffing function is combined together with folding/reducing functionality to allow convenient direct accumulation of changes.

### Set

Following example for string set accumulates all added values into list:

```rust
module StringSet = OcamlDiff.Set.Make(String);

let oldSet = StringSet.(empty |> add("A") |> add("B") |> add("C"));
let newSet =
  StringSet.(oldSet |> remove("A") |> remove("C") |> add("D") |> add("C"));
let addedItems =
  StringSet.symmetric_diff(oldSet, newSet, ~acc=[], ~f=(either, acc) =>
    switch (either) {
    | Left(a) =>
      Js.log("Element " ++ a ++ " is only in the old set.");
      acc;
    | Right(a) =>
      Js.log("Element " ++ a ++ " is only in the new set.");
      [a, ...acc];
    }
  );
// Element A is only in the old set.
// Element D is only in the new set.

Js.log(Array.of_list(addedItems));
// [ 'D' ]
```

### Map

Argument `~veq` specifies function for detecting equal values with signature `('value, 'value) => bool`.

Following example for string map accumulates all added values:

```rust
module StringMap = OcamlDiff.Map.Make(String);

let oldMap = StringMap.(empty |> add("A", 1) |> add("B", 2) |> add("C", 3));
let newMap =
  StringMap.(oldMap |> remove("A") |> add("D", 4) |> add("C", 5));

let addedItems =
  StringMap.symmetric_diff(
    oldMap, newMap, ~acc=[], ~veq=(==), ~f=(diffRes, acc) =>
    switch (diffRes) {
    | (key, Left(_a)) =>
      Js.log("Item with key " ++ key ++ " is only in the old map.");
      acc;
    | (key, Right(a)) =>
      Js.log("Item with key " ++ key ++ " is only in the new map.");
      [a, ...acc];
    | (key, Unequal(a, b)) =>
      Js.log(
        "Item with key "
        ++ key
        ++ " is in both, but its value was changed from "
        ++ string_of_int(a)
        ++ " to "
        ++ string_of_int(b)
        ++ ".",
      );
      acc;
    }
  );
// Item with key A is only in the old map.
// Item with key C is in both, but its value was changed from 3 to 5.
// Item with key D is only in the new map.

Js.log(Array.of_list(addedItems));
// [ 4 ]
```

## Development

### Build

```
npm run build
```

### Build + Watch

```
npm run start
```

### Run example

E.g.:

```
node lib/js/examples/MapDiffExample.js
```
