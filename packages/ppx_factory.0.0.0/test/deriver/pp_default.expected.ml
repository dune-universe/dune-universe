type _bool = bool[@@deriving default]
let default__bool = false
type _int = int[@@deriving default]
let default__int = 0
type _int32 = int32[@@deriving default]
let default__int32 = 0l
type int32_t = Int32.t[@@deriving default]
let default_int32_t = 0l
type _int64 = int64[@@deriving default]
let default__int64 = 0L
type int64_t = Int64.t[@@deriving default]
let default_int64_t = 0L
type _nativeint = nativeint[@@deriving default]
let default__nativeint = 0n
type nativeint_t = Nativeint.t[@@deriving default]
let default_nativeint_t = 0n
type _float = float[@@deriving default]
let default__float = 0.
type float_t = Float.t[@@deriving default]
let default_float_t = 0.
type _char = char[@@deriving default]
let default__char = '\000'
type char_t = Char.t[@@deriving default]
let default_char_t = '\000'
type _string = string[@@deriving default]
let default__string = ""
type string_t = String.t[@@deriving default]
let default_string_t = ""
type 'a _option = 'a option[@@deriving default]
let default__option = None
type 'a _list = 'a list[@@deriving default]
let default__list = []
type 'a _array = 'a array[@@deriving default]
let default__array = [||]
type tuple = (int * string * int list)[@@deriving default]
let default_tuple = (0, "", [])
type 'a ok = (int, 'a) result[@@deriving default]
let default_ok = Ok 0
type 'a error = ('a, 'a option) result[@@deriving default]
let default_error = Error None
module A =
  struct
    module B = struct type some_type = int
                      let default_some_type = 12 end
  end
type other_type = A.B.some_type[@@deriving default]
let default_other_type = A.B.default_some_type
type record =
  {
  int_field: int ;
  string_field: string ;
  other_field: A.B.some_type }[@@deriving default]
let default_record =
  { int_field = 0; string_field = ""; other_field = A.B.default_some_type }
type copied = record =
  {
  int_field: int ;
  string_field: string ;
  other_field: A.B.some_type }[@@deriving default]
let default_copied =
  { int_field = 0; string_field = ""; other_field = A.B.default_some_type }
type variant_without_arg =
  | A 
  | B of int [@@deriving default]
let default_variant_without_arg = A
type variant_single_arg =
  | A of int [@@deriving default]
let default_variant_single_arg = A 0
type variant_tuple_arg =
  | A of int * string [@@deriving default]
let default_variant_tuple_arg = A (0, "")
type variant_record_arg =
  | A of {
  int_field: int ;
  string_field: string } [@@deriving default]
let default_variant_record_arg = A { int_field = 0; string_field = "" }
type ('a, 'b) parametrized_variant =
  | A of 'a 
  | B of 'b 
  | C of 'b option [@@deriving default]
let default_parametrized_variant = C None
type poly_variant_without_arg = [ `A  | `B of int ][@@deriving default]
let default_poly_variant_without_arg = `A
type poly_variant_single_arg = [ `A of int  | `B ][@@deriving default]
let default_poly_variant_single_arg = `A 0
type poly_variant_tuple_arg = [ `A of (int * string)  | `B ][@@deriving
                                                              default]
let default_poly_variant_tuple_arg = `A (0, "")
type 'a poly_variant_open = [> `A  | `B ] as 'a[@@deriving default]
let default_poly_variant_open = `A
type ('a, 'b) parametrized_poly_variant =
  [ `A of 'a  | `B of 'b  | `C of 'b option ][@@deriving default]
let default_parametrized_poly_variant = `C None
module type DEFAULT  =
  sig
    type t[@@deriving default]
    val default : t
    type simple[@@deriving default]
    val default_simple : simple
    type with_manifest = int[@@deriving default]
    val default_with_manifest : with_manifest
    type private_ = private int[@@deriving default]
    val default_private_ : private_
    type ('a, 'b) parametrized[@@deriving default]
    val default_parametrized : ('a, 'b) parametrized
    type variant =
      | A of int 
      | B of string [@@deriving default]
    val default_variant : variant
    type record = {
      a: int ;
      b: string }[@@deriving default]
    val default_record : record
    type copied = record = {
      a: int ;
      b: string }[@@deriving default]
    val default_copied : copied
  end
