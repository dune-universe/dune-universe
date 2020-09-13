type t = { user : string; repo : string; branch : string }

val pp : Format.formatter -> t -> unit

val git_url : t -> string
