# Compilation of ML values to Michelson

SCaml Tuples, records, variants and entry points are compiled into the binary data structures of Michelson: pairs and eithers.  For the efficiency, the data must be stored as balanced as possible.  SCaml uses **right balanced binary trees**: components declared earlier are placed 1 level shallower than the others, therefore they are slightly more efficient for access.

For the general idea of the right balanced tree layout, see TZIP-6 called [A1.1](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-6/tzip-6.md).

The variant compilation uses the same right balanced binary tree but with a hack for nullary constructors.  They are squashed to the left most element of the right balanced tree with a tag of a natural number.  This is inspired by the variant representation of OCaml.

For example the constructors of the following variant type:

```ocaml
type t =
  | A
  | B of int
  | C of nat * string
  | D
  | E of address
  | F of tz
```

are arranged in the following right balanced tree:

```
-+- left  -+- left  --- int, 0 for A and 1 for D
 |         | 
 |         \- right --- int, for B
 |
 \- right -+- left --- nat * strint, for C
           |
           \- right -+- left  --- address, for E
                     |
                     \- right --- tz, for F
```

This may be **incompatible** with the ways the other languages use.
