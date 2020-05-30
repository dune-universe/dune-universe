val used_words : unit -> int
val with_compacts : ('a -> 'b) -> 'a -> 'b * (int * int)
(** Returns used words before and after *)

