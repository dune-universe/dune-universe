open Batteries;;

(** This exception should be raised when code has not yet been implemented. *)
exception Not_yet_implemented of string;;

(** This exception should be raised if a defensive runtime check determines that
    an invariant has been violated. *)
exception Invariant_failure of string;;

val natural_compare_seq : ((unit -> int) list) -> int

val uniq_enum : ('a -> 'a -> int) -> 'a Enum.t -> 'a Enum.t

(**
   Expands a list of lists as a Cartesian product.  That is, the list
   {[
     [[1;2;3];[4];[5;6]]
   ]}
   would yield the list
   {[
     [[1;4;5];[2;4;5];[3;4;5];[1;4;6];[2;4;6];[3;4;6]]
   ]}
   the first element being the result of selecting [1], [4], and [5] from the
   original three lists.
*)
val cartesian_product_of_list : 'a list list -> 'a list list

(**
  A pairwise pseudo-map over an enum.  For each successive pair of values in the
  enum, the map function is called.  So,
  {[  pairwise_enum_fold f (List.enum [1;2;3;4])  ]}
  is equivalent to
  {[  List.enum [f 1 2; f 2 3; f 3 4]  ]}
  This is equivalent to cloning the enum, discarding the first element from the
  clone, zipping the resulting enums, and mapping over the result.
*)
val pairwise_enum_fold : ('a -> 'a -> 'b) -> 'a Enum.t -> 'b Enum.t

(**
  Sets the contents of the indicated file to the provided string.
*)
val set_file_contents : string -> string -> unit