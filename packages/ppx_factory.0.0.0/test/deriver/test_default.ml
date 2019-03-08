type _bool = bool [@@deriving default]
type _int = int [@@deriving default]
type _int32 = int32 [@@deriving default]
type int32_t = Int32.t [@@deriving default]
type _int64 = int64 [@@deriving default]
type int64_t = Int64.t [@@deriving default]
type _nativeint = nativeint [@@deriving default]
type nativeint_t = Nativeint.t [@@deriving default]
type _float = float [@@deriving default]
type float_t = Float.t [@@deriving default]
type _char = char [@@deriving default]
type char_t = Char.t [@@deriving default]
type _string = string [@@deriving default]
type string_t = String.t [@@deriving default]
type 'a _option = 'a option [@@deriving default]
type 'a _list = 'a list [@@deriving default]
type 'a _array = 'a array [@@deriving default]
type tuple = int * string * int list [@@deriving default]

type 'a ok = (int, 'a) result [@@deriving default]
type 'a error = ('a, 'a option) result [@@deriving default]

module A = struct
  module B = struct
    type some_type = int
    let default_some_type = 12
  end
end

type other_type = A.B.some_type [@@deriving default]

type record =
  { int_field : int
  ; string_field : string
  ; other_field : A.B.some_type
  }
[@@deriving default]

type copied = record =
  { int_field : int
  ; string_field : string
  ; other_field : A.B.some_type
  }
[@@deriving default]

type variant_without_arg =
  | A
  | B of int
[@@deriving default]

type variant_single_arg =
  | A of int
[@@deriving default]

type variant_tuple_arg =
  | A of int * string
[@@deriving default]

type variant_record_arg =
  | A of {int_field : int; string_field : string}
[@@deriving default]

type ('a, 'b) parametrized_variant =
  | A of 'a
  | B of 'b
  | C of 'b option
[@@deriving default]

type poly_variant_without_arg = [`A | `B of int]
[@@deriving default]

type poly_variant_single_arg = [`A of int | `B]
[@@deriving default]

type poly_variant_tuple_arg = [`A of int * string | `B]
[@@deriving default]

type 'a poly_variant_open = [>`A | `B] as 'a
[@@deriving default]

type ('a, 'b) parametrized_poly_variant = [`A of 'a | `B of 'b | `C of 'b option]
[@@deriving default]

module type DEFAULT = sig
  type t [@@deriving default]
  type simple [@@deriving default]
  type with_manifest = int [@@deriving default]
  type private_ = private int [@@deriving default]
  type ('a, 'b) parametrized [@@deriving default]
  type variant = A of int | B of string [@@deriving default]
  type record = {a : int; b : string} [@@deriving default]
  type copied = record = {a : int; b : string} [@@deriving default]
end
