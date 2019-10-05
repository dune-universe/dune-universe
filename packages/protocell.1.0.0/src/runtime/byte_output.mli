type t

val create : ?initial_size:int -> unit -> t

val write_byte : t -> char -> unit

val write_bytes : t -> string -> unit

val write_bytes' : t -> bytes -> unit

val contents : t -> string
