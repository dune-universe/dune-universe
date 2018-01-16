(** {1} Construction *)
  
val make1 : char -> bytes
(** [make1 = Bytes.make 1] *) 

val of_char : char -> bytes
(** Sysnonym [make1] *) 

(** {1} Deconstruction *)

val to_array : bytes -> char array

val to_code_array : bytes -> int array

(** {1} Array *)

val get_opt : bytes -> int -> char option

val scani_left : 
  (int -> 'a -> char -> [< `Continue of 'a | `Stop of 'a]) 
  -> 'a -> ?from:int -> ?to_:int -> bytes -> 'a

val foldi_left : 
  (int -> 'a -> char -> [< `Continue of 'a | `Stop of 'a]) 
  -> 'a -> bytes -> 'a

val replace_chars : char -> char -> bytes -> bytes
(** [replace_chars c1 c2 s] returns a copy of [s] with replacing
    all the char occurrences of [c1] by [c2]. *)

(** {1} Transform *)

val chop_eols : bytes -> bytes
(** [chop_eols s] returns the bytes [s] w/o the end-of-line chars. [chop] from Perl.
    [chop_eols "hello\r\n" = "hello"]
    [chop_eols "hello\n" = "hello"]
    [chop_eols "hello\r" = "hello"]
    [chop_eols "hello" = "hello"]
*)

(** {1} Sub *)

val sub_from_to : bytes -> int -> int -> bytes

val sub' : bytes -> int -> int -> bytes
(** Same as [Bytes.sub] but even if the bytes shorter for [len] 
    the function succeeds and returns a shorter subbytes. 
*)

val is_sub : ?from:int -> needle:bytes -> bytes -> bool

val is_prefix : ?from:int -> bytes -> bytes -> bool
val is_postfix : bytes -> bytes -> bool

val is_prefix' : ?from:int -> bytes -> bytes -> bytes option
(** Same as [prefix] but returns the postfix *)

val is_postfix' : bytes -> bytes -> bytes option
(** Same as [postfix] but returns the prefix *)

val index_from_to : bytes -> int -> int -> char -> int option
val index_bytes_from : bytes -> int -> bytes -> int (* may raise Not_found *)

(** Haskelish bytes sub *)
val split_at : int -> bytes -> bytes * bytes
val take : int -> bytes -> bytes
val drop : int -> bytes -> bytes
val drop_postfix : int -> bytes -> bytes

val prefix : int -> bytes -> bytes
(** same as take *)

val postfix : int -> bytes -> bytes

(** {1} Split *)
  
val lines : bytes -> (bytes * bytes) list
(** [lines "hello\nworld\r\ngood\rday" = ["hello", "\n"; "world", "\r\n"; "good", "\r"; "day", ""]]  *)

val split : (char -> bool) -> bytes -> bytes list
(** [split (function ' ' -> true | _ -> false) "hello      world" = ["hello"; "world"]] *) 

val split1 : ?from:int -> (char -> bool) -> bytes -> (bytes * bytes) option
(** Same as [split] but do the split only once *)

val words : bytes -> bytes list
(** Split a bytes into "words" by white characters [' '], ['\t'], ['\r']
    and ['\n']
*)
  
(** Optionalized *)
val index_opt : bytes -> char -> int option

val find : bytes -> int -> (char -> bool) -> int option

(** {1} Random *)

val random : int (*+ length *) -> bytes

val random_hum : int (*+ length *) -> bytes
(** human readable *)

(** {1} Misc *)
  
val is_space_or_tab : char -> bool
val is_newline_or_return : char -> bool

(** {1} Pervasives *)

module Pervasives : sig
  val chop_eols : bytes -> bytes
end

(** {1} Set *)
  
module Set : Xset.S with type elt = Bytes.t
