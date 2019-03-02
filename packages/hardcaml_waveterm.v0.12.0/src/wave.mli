open! Import

type t =
  | Clock of string
  | Binary of string * Data.t
  | Data of string * Data.t * Wave_format.t
[@@deriving sexp_of]

val get_name : t -> string
val get_data : t -> Data.t
val get_to_str : t -> Bits.t -> string
val get_format : t -> Wave_format.t
