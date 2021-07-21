type t = {
  chr : string ;
  lo : int ;
  hi : int ;
} [@@deriving equal, compare, sexp]

val of_string : string -> (t, [> `Parse_error]) result
val of_string_exn : string -> t

val to_string : t -> string
(** String representation of a location, as <chr>:<start>-<end> *)

val range : t -> Range.t

val size : t -> int

val strictly_before : t -> t -> bool

val intersects : t -> t -> bool

val included_in : t -> t -> bool

val inter : t -> t -> t option

val relmove : t -> int -> int -> t

val relative : t -> int -> int -> t
(** [relative loc x a b] is the location obtained considering [a] and
    [b] as relative coordinates inside [loc] *)

val zoom : t -> float -> t
(** [zoom l z] is a location of width [z *. float (Location.width l)] *)

val dist : t -> t -> int option

val position : from:t -> t -> int option

val union : t -> t -> t option

module Set : Core_kernel.Set.S with type Elt.t := t
module Map : Core_kernel.Map.S with type Key.t := t
