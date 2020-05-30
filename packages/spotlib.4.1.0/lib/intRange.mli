type t (** integer range *)

val empty : t (** empty range *)

val is_empty : t -> bool
  
val make : int -> int -> t 
(** Make a range between 2 integers, inclusive. 
    If the second int is smaller than the first, [make] returns [empty]. 
*)  

val iter : (int -> unit) -> t -> unit
(** Iteration *)
  
val map : (int -> 'a) -> t -> 'a list
(** Map to list *)
  
val fold_left : ('acc -> int -> 'acc) -> 'acc -> t -> 'acc
(** Folding from left *)

val range : t -> (int * int) option
(** Get the integer range.  Returns [None] if the range is empty *)
    
