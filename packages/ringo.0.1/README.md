# Ringo

Ring (bounded-size data collection with first-added first-thrown-away policy)
and derived data-structure

## `Ring`

A ring is a data-structure (`'a Ring.t`) holding elements of a given type
(`'a`). Elements can be added to it (`Ring.add ring v`). Only a bounded number
of elements are held (set when initialising an empty ring with
`Ring.create bound`). When the ring is full, adding a new element discards the
oldest element.

## `Ring.MakeTable`

A ring-table is a table-like (`Hashtbl`-like) data-structure that only retains a
bounded number of elements.

## `WeakRingTable.Make`

A weak-ring-table is a ring-table-like data-structure that is sloppy in counting
elements: it occasionally contains more elements than the bound (although
supernumerary elements are only weakly held and can be collected by the GC) and
might contain less.

