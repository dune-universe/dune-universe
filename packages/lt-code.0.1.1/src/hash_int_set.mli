type t

val create : int -> t

val add : t -> int -> unit

val mem : t -> int -> bool

val remove : t -> int -> unit

val reset : t -> unit

val cardinal : t -> int

val iter : (int -> unit) -> t -> unit

val choose_opt : t -> int option

val choose : t -> int
