(** HTTP headers *)
type t =
  [ `Content_length of int
  | `Content_type of string
  | `Location of Uri.t
  | `Set_cookie of string (* Use some other library, like cohttp to create *)
  | `Other of string * string
  | `Status of Http_status.t ]

val to_string : t -> string
