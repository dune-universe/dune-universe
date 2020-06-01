open! Import

type t =
  | Empty of string
  | Clock of string
  | Binary of string * Data.t
  | Data of string * Data.t * Wave_format.t * Wave_format.alignment
[@@deriving sexp_of]

val set_name : t -> string -> t
val get_name : t -> string
val get_data : t -> Data.t
val get_to_str : t -> Bits.t -> string
val get_alignment : t -> Wave_format.alignment
val get_format : t -> Wave_format.t
