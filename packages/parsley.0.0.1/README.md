# Parsley
Parsley provides utilities to convert a string representation of an
OCaml numeric value (float, int32, int64, native) to the corresponding
value while indicating if a loss of precision occured during the
conversion.

### How?
Parsley parses the string using arbitrary precision integers and
rationals (using the Zarith library) and compares the result it
obtains with the one obtained by OCaml's standard of_string utilities
(int_of_string, float_of_string ...)

### PPX
You can use Parsley's functionnalities on your OCaml source code using
the ppx that goes with the library. It generates pretty warning messages
using OCaml's warning styles as in the following examples.

##### todo example

## Dependencies
- Zarith: https://github.com/ocaml/Zarith

## Compatibility:
- OCaml >= 4.08
