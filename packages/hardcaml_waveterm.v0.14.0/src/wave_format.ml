open! Import

type alignment =
  | Left
  | Right
[@@deriving sexp_of]

type t =
  | Binary
  | Bit
  | Bit_or of t
  | Hex
  | Unsigned_int
  | Int
  | Index of string list
  | Custom of (Bits.t -> string)
[@@deriving sexp_of]

let rec equal a b =
  match a, b with
  | Binary, Binary | Bit, Bit | Hex, Hex | Int, Int | Unsigned_int, Unsigned_int -> true
  | Bit_or a, Bit_or b -> equal a b
  | Index a, Index b -> [%compare.equal: string list] a b
  | Custom f, Custom g -> phys_equal f g
  | (Bit | Bit_or _ | Binary | Hex | Unsigned_int | Int | Index _ | Custom _), _ -> false
;;
