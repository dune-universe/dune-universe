# UnionFind: implementations of the union-find data structure

The OCaml library `unionFind` offers two implementations of Tarjan's
union-find data structure. Both implementations use path compression and
linking-by-rank. The direct implementation uses heap-allocated mutable state.
The indirect implementation is parameterized over an implementation of the
signature `STORE` ([link](src/Store.ml)), and its operations explicitly take and
return a store, so it is possible to use a persistent store, a
store-in-an-array, etc.

## Installation

To install the latest released version,
type `opam install unionFind`.

To install from source,
clone this repository
and type `make install`.

## Overview of the direct implementation

This implementation ([link](src/UnionFindBasic.mli))
offers a type `'a UnionFind.elem` of "elements" and
a number of functions that manipulate elements,
including `UnionFind.make`, `UnionFind.union`, `UnionFind.find`, and so on.

## Overview of the indirect implementation

This implementation ([link](src/UnionFindOverStore.mli)) offers a functor
`UnionFind.Make` whose input is an implementation of the signature `STORE`
([link](src/Store.ml)). Its output is another implementation of the signature
`STORE`, which in addition to the standard operations on references also
offers a `union` operation.

A number of implementations of the signature `STORE` are provided:

* `UnionFind.StoreMap` ([link](src/StoreMap.mli)) implements a store as an
  immutable integer map. Thus, `UnionFind.Make(UnionFind.StoreMap)` offers a
  persistent union-find data structure.

* `UnionFind.StoreRef` ([link](src/StoreRef.mli)) implements a store in terms
  of heap-allocated mutable state. There is no compelling reason to use
  `UnionFind.Make(UnionFind.StoreRef)`, because the direct implementation
  `UnionFind` is more efficient.

* `UnionFind.StoreTransactionalRef` ([link](src/StoreTransactionalRef.mli))
  implements a mutable store that supports a form of transaction. Thus,
  `UnionFind.Make(UnionFind.StoreTransactionalRef)` offers a union-find data
  structure that also supports transactions.

  The higher-order function `UnionFind.StoreTransactionalRef.tentatively` is
  used to delimit the scope of a transaction. `tentatively s f` runs the
  function `f` within a new transaction on the store `s`. If `f` raises an
  exception, then the transaction is aborted, and any updates performed by `f`
  are rolled back. Otherwise, the updates performed by `f` are committed.

* `UnionFind.StoreVector` ([link](src/StoreVector.mli)) implements a store as
  a mutable vector, that is, a mutable extensible array. Thus,
  `UnionFind.Make(UnionFind.StoreVector)` offers an ephemeral union-find data
  structure, just like `UnionFind.Make(UnionFind.StoreRef)` and `UnionFind`.
