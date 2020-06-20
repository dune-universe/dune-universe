# Changes

## 2020/06/18

* The time complexity of every operation is now documented as part of its
  specification.

* New submodules `E.Iter` and `P.Iter`, which offer very efficient iterators
  over ephemeral and persistent sequences.

* New functions
  `take`, `drop`, `sub`,
  `iter_segments`,
  `to_seq`,
  `of_list_segment`, `of_list`, `of_seq_segment`, `of_seq`,
  `find`, `find_opt`, `find_map`, `for_all`, `exists`, `mem`, `memq`,
  `map`, `mapi`, `rev`, `zip`, `unzip`,
  `filter`, `filter_map`, `partition`,
  `flatten`, `flatten_map`,
  `iter2`, `iter2_segments`, `fold_left2`, `fold_right2`,
  `map2`, `for_all2`, `exists2`, `equal`, `compare`,
  `sort`, `stable_sort`, `uniq`, `merge`,
  in ephemeral and persistent flavors.

* New functions `E.fill` and `E.blit`.

* New function `other` of type `side -> side`.

* New function `opposite` of type `direction -> direction`.

* New functor `SupplyDefault`, which allows supplying a default element
  once and for all so as to obtain a simpler API. Unfortunately, this
  requires choosing a fixed type of elements at the same time.

* New submodules `Emulated.Array` and `Emulated.List`, which can be used
  as drop-in replacements for OCaml's standard `Array` and `List` modules.

* New submodule `Segment`, which offers a few facilities for iterating
  over array segments.

* **Breaking change:** the default behavior of `E.copy` is now to produce a
  disjoint sequence in time `O(n)`. The previous behavior, which exploits
  sharing and produces a result in time `O(K)`, is obtained by invoking
  ``E.copy ~mode:`Share``. The two copying modes have the same observable
  behavior; they differ only in their performance characteristics.

* **Breaking change:** the submodules `Queue` and `Stack` are renamed
  `Emulated.Queue` and `Emulated.Stack`. This makes it easier to avoid
  unintended shadowing of `Stdlib.Queue`, `Stdlib.Stack`, etc. It is now
  safe to use `open Sek`.

* **Breaking change:** the functor `Make` now takes just one structure as a
  parameter, instead of several structures. This is more pleasant and should
  make future evolution easier.

## 2020/04/03

* Initial release of the library.
