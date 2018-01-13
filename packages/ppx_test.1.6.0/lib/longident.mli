type t =
  | Lident of string
  | Ldot of t * string
  | Lapply of t * t

val to_string : t -> string

val concat : t -> t -> t
