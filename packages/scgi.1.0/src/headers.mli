(** SCGI request headers *)
type t = (string * string) list

val of_string : string -> t
