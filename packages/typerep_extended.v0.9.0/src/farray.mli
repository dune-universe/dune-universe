open! Core_kernel

type 'a t [@@deriving sexp, bin_io, typerep]

val get         : 'a t -> int -> 'a
val length      : _ t -> int
val init        : int -> f:(int -> 'a) -> 'a t
val empty       : unit -> 'a t
val make1       : 'a -> 'a t
val make2       : 'a -> 'a -> 'a t
val make3       : 'a -> 'a -> 'a -> 'a t
val make4       : 'a -> 'a -> 'a -> 'a -> 'a t
val make5       : 'a -> 'a -> 'a -> 'a -> 'a -> 'a t
val find        : 'a t -> f:('a -> bool) -> 'a option
val iter        : 'a t -> f:('a -> unit) -> unit
val iteri       : 'a t -> f:(int -> 'a -> unit) -> unit
val map         : 'a t -> f:('a -> 'b) -> 'b t
val mapi        : 'a t -> f:(int -> 'a -> 'b) -> 'b t
val sort        : 'a t -> cmp:('a -> 'a -> int) -> 'a t
val for_all     : 'a t -> f:('a -> bool) -> bool
val exists      : 'a t -> f:('a -> bool) -> bool
val copy        : 'a array -> 'a t
val of_array    : 'a array -> f:(int -> 'a -> 'b) -> 'b t
val to_array    : 'a t -> f:(int -> 'a -> 'b) -> 'b array
val map_stable  : 'a t -> f:('a -> 'a) -> 'a t
val to_list     : 'a t -> 'a list
val of_list     : 'a list -> 'a t
val iter2_exn   : 'a t -> 'b t -> f:('a -> 'b -> unit) -> unit
val is_empty    : _ t -> bool
val of_list_map : 'a list -> f:('a -> 'b) -> 'b t
val to_list_map : 'a t -> f:('a -> 'b) -> 'b list
