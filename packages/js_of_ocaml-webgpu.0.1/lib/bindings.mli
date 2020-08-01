open Core

type t

val create : Webidl.Parse.data -> t
val mli : t -> string list
val ml : t -> string list
val errors : t -> Error.t list
