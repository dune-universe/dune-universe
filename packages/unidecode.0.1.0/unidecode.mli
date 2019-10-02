val nbc : char -> int

val decode
  : (int -> string -> int -> int -> 'a)
  -> (int -> char -> 'a)
  -> (int -> string -> int -> int -> 'a)
  -> string -> int -> int -> 'a
