Cosovo is an OCaml package, providing a CSV parser.  Cosovo's parser
includes support for

 * sparse rows
 * header and header-less inputs
 * integer, floating point, and string values
 * comments

By way of example, consider the following valid input:

```
a,b,c
1,2,3,4
{2 "this is a string value associated with the third column", 0 -1e-10} # long line!
# this is a comment, followed by several empty lines


# another comment
"foo"
{1000 1,2001 99.9}
```

The command-line tool `csvcat` validates and echos its input,
stripping newlines and comments, if any (see `csvcat --help`).

The interface to the package is rather simple:
```
let ch = open_in "myfile.csv" ;;
let header, row_seq =
  match Cosovo.IO.of_channel ch with
  | Ok h_rs -> h_rs
  | Error _ -> (* parse errors *)
in
Seq.iter (
  function
  | Ok (`Dense dense)   -> (* do something with dense row  *)
  | Ok (`Sparse sparse) -> (* do something with sparse row *)
  | Error _             -> (* errors *)
) row_seq
;;
```

The type of a row is:
```
type row = [ `Dense of dense | `Sparse of sparse ]
```

where

```
type value = [ `Int of int | `Float of float | `String of string ]
type dense = value list
type sparse = (int * value) list
```

The first element of each sparse cell - an integer - is the 0-based
index of its corresponding column.  In the example above, the value
associated with the 2002-th column in the last row is `99.9` .

It is up to the user as to whether to interpret the first row as a
data row or a header.

To install, use opam:
```
opam install cosovo
```
