# ppx_type_of: to get type string of an expression

`Ppx_type_of` replaces

```
Ppx_type_of.type_of e
```

by the string of the type of `e`.

## Example

See `tests/test.ml`.

## Requirement

To use `ppx_type_of` correctly you must define a module `Ppx_type_of` with
the following definition:

```
let type_of _ : string = raise (Failure "you must use ppx_type_of")
```

## Note

Expression `Ppx_type_of.type_of e` is replaced by a string constant
and the original `e` is removed from the source code. Therefore
`e`'s side effects are never performed. The uses of names inside `e`
are also removed and this removal may cause unused variable warnings.

```
open Ppx_type_of

let () = assert (type_of 1 = "int")
let () = assert (type_of None = "'a option")
let () = assert (type_of (None : int option) = "int option")
```
