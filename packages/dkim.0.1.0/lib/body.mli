type decode = [ `Data of string | `Await | `End | `CRLF | `Spaces of string ]

type decoder

val src : decoder -> bytes -> int -> int -> unit

val decode : decoder -> decode

val decoder : unit -> decoder
