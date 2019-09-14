(** See {!Dropbox.Date} for documentation. *)

type t

type wday = Sun | Mon | Tue | Wed | Thu | Fri | Sat

type month =
    Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

val of_string : string -> t
val to_string : t -> string


val day : t -> int
val month : t -> month
val year : t -> int
val hour : t -> int
val min : t -> int
val sec : t -> int
val wday : t -> wday
;;
