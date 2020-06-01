type _bool = bool[@@deriving default]
include struct let default__bool = false end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                    ]
type _int = int[@@deriving default]
include struct let default__int = 0 end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                ]
type _int32 = int32[@@deriving default]
include struct let default__int32 = 0l end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                   ]
type int32_t = Int32.t[@@deriving default]
include struct let default_int32_t = 0l end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                    ]
type _int64 = int64[@@deriving default]
include struct let default__int64 = 0L end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                   ]
type int64_t = Int64.t[@@deriving default]
include struct let default_int64_t = 0L end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                    ]
type _nativeint = nativeint[@@deriving default]
include struct let default__nativeint = 0n end[@@ocaml.doc "@inline"]
[@@merlin.hide ]
type nativeint_t = Nativeint.t[@@deriving default]
include struct let default_nativeint_t = 0n end[@@ocaml.doc "@inline"]
[@@merlin.hide ]
type _float = float[@@deriving default]
include struct let default__float = 0. end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                   ]
type float_t = Float.t[@@deriving default]
include struct let default_float_t = 0. end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                    ]
type _char = char[@@deriving default]
include struct let default__char = '\000' end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                    ]
type char_t = Char.t[@@deriving default]
include struct let default_char_t = '\000' end[@@ocaml.doc "@inline"]
[@@merlin.hide ]
type _string = string[@@deriving default]
include struct let default__string = "" end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                    ]
type string_t = String.t[@@deriving default]
include struct let default_string_t = "" end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                    ]
type 'a _option = 'a option[@@deriving default]
include struct let default__option = None end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                    ]
type 'a _list = 'a list[@@deriving default]
include struct let default__list = [] end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                  ]
type 'a _array = 'a array[@@deriving default]
include struct let default__array = [||] end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                    ]
type tuple = (int * string * int list)[@@deriving default]
include struct let default_tuple = (0, "", []) end[@@ocaml.doc "@inline"]
[@@merlin.hide ]
type 'a ok = (int, 'a) result[@@deriving default]
include struct let default_ok = Ok 0 end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                 ]
type 'a error = ('a, 'a option) result[@@deriving default]
include struct let default_error = Error None end[@@ocaml.doc "@inline"]
[@@merlin.hide ]
module A =
  struct
    module B = struct type some_type = int
                      let default_some_type = 12 end
  end
type other_type = A.B.some_type[@@deriving default]
include struct let default_other_type = A.B.default_some_type end[@@ocaml.doc
                                                                   "@inline"]
[@@merlin.hide ]
type record =
  {
  int_field: int ;
  string_field: string ;
  other_field: A.B.some_type }[@@deriving default]
include
  struct
    let default_record =
      { int_field = 0; string_field = ""; other_field = A.B.default_some_type
      }
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type copied = record =
  {
  int_field: int ;
  string_field: string ;
  other_field: A.B.some_type }[@@deriving default]
include
  struct
    let default_copied =
      { int_field = 0; string_field = ""; other_field = A.B.default_some_type
      }
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type variant_without_arg =
  | A 
  | B of int [@@deriving default]
include struct let default_variant_without_arg = A end[@@ocaml.doc "@inline"]
[@@merlin.hide ]
type variant_single_arg =
  | A of int [@@deriving default]
include struct let default_variant_single_arg = A 0 end[@@ocaml.doc
                                                         "@inline"][@@merlin.hide
                                                                    ]
type variant_tuple_arg =
  | A of int * string [@@deriving default]
include struct let default_variant_tuple_arg = A (0, "") end[@@ocaml.doc
                                                              "@inline"]
[@@merlin.hide ]
type variant_record_arg =
  | A of {
  int_field: int ;
  string_field: string } [@@deriving default]
include
  struct
    let default_variant_record_arg = A { int_field = 0; string_field = "" }
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type ('a, 'b) parametrized_variant =
  | A of 'a 
  | B of 'b 
  | C of 'b option [@@deriving default]
include struct let default_parametrized_variant = C None end[@@ocaml.doc
                                                              "@inline"]
[@@merlin.hide ]
type poly_variant_without_arg = [ `A  | `B of int ][@@deriving default]
include struct let default_poly_variant_without_arg = `A end[@@ocaml.doc
                                                              "@inline"]
[@@merlin.hide ]
type poly_variant_single_arg = [ `A of int  | `B ][@@deriving default]
include struct let default_poly_variant_single_arg = `A 0 end[@@ocaml.doc
                                                               "@inline"]
[@@merlin.hide ]
type poly_variant_tuple_arg = [ `A of (int * string)  | `B ][@@deriving
                                                              default]
include struct let default_poly_variant_tuple_arg = `A (0, "") end[@@ocaml.doc
                                                                    "@inline"]
[@@merlin.hide ]
type 'a poly_variant_open = [> `A  | `B ] as 'a[@@deriving default]
include struct let default_poly_variant_open = `A end[@@ocaml.doc "@inline"]
[@@merlin.hide ]
type ('a, 'b) parametrized_poly_variant =
  [ `A of 'a  | `B of 'b  | `C of 'b option ][@@deriving default]
include struct let default_parametrized_poly_variant = `C None end[@@ocaml.doc
                                                                    "@inline"]
[@@merlin.hide ]
module type DEFAULT  =
  sig
    type t[@@deriving default]
    include sig val default : t end[@@ocaml.doc "@inline"][@@merlin.hide ]
    type simple[@@deriving default]
    include sig val default_simple : simple end[@@ocaml.doc "@inline"]
    [@@merlin.hide ]
    type with_manifest = int[@@deriving default]
    include sig val default_with_manifest : with_manifest end[@@ocaml.doc
                                                               "@inline"]
    [@@merlin.hide ]
    type private_ = private int[@@deriving default]
    include sig val default_private_ : private_ end[@@ocaml.doc "@inline"]
    [@@merlin.hide ]
    type ('a, 'b) parametrized[@@deriving default]
    include sig val default_parametrized : ('a, 'b) parametrized end[@@ocaml.doc
                                                                    "@inline"]
    [@@merlin.hide ]
    type variant =
      | A of int 
      | B of string [@@deriving default]
    include sig val default_variant : variant end[@@ocaml.doc "@inline"]
    [@@merlin.hide ]
    type record = {
      a: int ;
      b: string }[@@deriving default]
    include sig val default_record : record end[@@ocaml.doc "@inline"]
    [@@merlin.hide ]
    type copied = record = {
      a: int ;
      b: string }[@@deriving default]
    include sig val default_copied : copied end[@@ocaml.doc "@inline"]
    [@@merlin.hide ]
  end
