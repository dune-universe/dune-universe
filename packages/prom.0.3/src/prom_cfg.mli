type ezjsonm =
  [ `O of (string * ezjsonm) list
  | `A of ezjsonm list
  | `Bool of bool
  | `Float of float
  | `String of string
  | `Null ]

type yojson =
  [ `Bool of bool
  | `Assoc of (string * yojson) list
  | `Float of float
  | `Int of int
  | `Intlit of string
  | `List of yojson list
  | `Null
  | `String of string
  | `Tuple of yojson list
  | `Variant of string * yojson option ]

module File_sd_config : sig
  type t = { targets : host list; labels : (string * string) list }

  and host = string * int option

  val create : ?labels:(string * string) list -> host list -> t

  val to_ezjsonm : t list -> ezjsonm

  val to_yojson : t list -> yojson
end
