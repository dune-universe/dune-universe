type t = { user : string; repo : string; number : int }

val pp : Format.formatter -> t -> unit

val equal : t -> t -> bool

val parse : string -> t option

val user_re : Re.t

val repo_re : Re.t
