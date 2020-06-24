type value = [ `Int of int | `Float of float | `String of string ]
type dense = value list
type sparse = (int * value) list
type row = [ `Dense of dense | `Sparse of sparse | `EOF ]
type header = [ `Dense of string list | `Sparse of (int * string) list ]
type opt_row = value option list
