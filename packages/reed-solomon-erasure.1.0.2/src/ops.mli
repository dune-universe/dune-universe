val ( .%()   ) : bytes -> int -> char
val ( .%()<- ) : bytes -> int -> char -> unit

val ( .%{}   ) : bytes -> int -> char
val ( .%{}<- ) : bytes -> int -> char -> unit

val ( .%[]   ) : string -> int -> char

val ( .&{}   ) : Core_kernel.Bigstring.t -> int -> int
val ( .&{}<- ) : Core_kernel.Bigstring.t -> int -> int -> unit
