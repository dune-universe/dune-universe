type t = { r: int; g: int; b: int } [@@deriving sexp, fields]
val rand : unit -> t
val black : t
val white : t
val green : t
val red : t
val blue : t
val purple : t
val of_hex_int : int -> t
val to_gl : t -> (float * float * float)
val of_gl : (float * float * float) -> t
val to_string : t -> string
val shade : t -> factor:float -> t
val of_string : string -> t
