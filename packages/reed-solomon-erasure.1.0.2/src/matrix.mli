type t

module Error : sig
  type t = SingularMatrix

  module Exn : sig
    exception SingularMatrix
  end

  val to_exn : t -> exn

  val unwrap : ('a, t) result -> 'a
end

val ( .%{}   ) : bytes array -> int * int -> char
val ( .%{}<- ) : bytes array -> int * int -> char -> unit

val ( .&{}   ) : t -> int * int -> char
val ( .&{}<- ) : t -> int * int -> char -> unit

val make_bytes_array : int -> int -> bytes array

val make : int -> int -> t

val make_with_data : bytes array -> t

val print_debug : t -> unit

val identity : int -> t

val col_count : t -> int

val row_count : t -> int

val get : t -> int -> int -> char

val set : t -> int -> int -> char -> unit

val copy : t -> t

val multiply : t -> t -> t

val augment : t -> t -> t

val sub_matrix : t -> int -> int -> int -> int -> t

val get_row : t -> int -> bytes

val swap_rows : t -> int -> int -> unit

val is_square : t -> bool

val gaussian_elim : t -> (unit, Error.t) result

val invert : t -> (t, Error.t) result

val vandermonde : int -> int -> t
