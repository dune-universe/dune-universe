(** Miscellaneous *)

(** Does string start with an uppercase letter? *)
val is_upper : string -> bool

(** Capitalize string *)
val cap : string -> string

(** Uncapitalize string *)
val uncap : string -> string

(** A stream *)
type 'a stream = Nil | Cons of 'a * (unit -> 'a stream)
