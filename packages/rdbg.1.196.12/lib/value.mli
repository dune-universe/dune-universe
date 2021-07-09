(* Time-stamp: <modified the 27/04/2018 (at 11:06) by Erwan Jahier> *)
(*-----------------------------------------------------------------------
** File: value.mli
** Author: Erwan Jahier
*)

(**)
(** Internal representation of values (bool, int, floats) manipulated
  in lurette. *)

type num = I of Num.num | F of float
type t = B of bool | N of num

val to_data_val : t -> Data.v
val from_data_val : Data.v -> t

val num_is_int: num -> bool

(** Various operations on num values. *)
val mult_num   : num -> num -> num
val diff_num   : num -> num -> num
val add_num    : num -> num -> num
val div_num    : num -> num -> num
val modulo_num : num -> num -> num

(* the same as div_num, except that is raises an internal error if 
   the second arg does not divide the first *)
val quot_num   : num -> num -> num


(** [num_eq_zero ne] returns true iff [ne] = 0 *)
val num_eq_zero : num -> bool

(** [num_sup_zero ne] returns true iff [ne] > 0 *)
val num_sup_zero : num -> bool

(** [num_supeq_zero ne] returns true iff [ne] >= 0 *)
val num_supeq_zero : num -> bool

(** [neg ne] returns [-ne] *)
val neg : num -> num


(**/**)

(** Pretty printing. *)
val print : out_channel -> t -> unit
val num_value_to_string : num -> string
val to_string : t -> string
val list_to_string : t list -> string -> string


(* Assoc string -> value *)
type name = string
module OfIdent :
  sig
    type value = t
    type t
    val get : t -> name -> value
    val add : t -> (name * value) -> t
    val add2 : t -> name -> value -> t
    val add_list : t -> (name * value) list -> t
    val add_list2 : t -> name list -> value list -> t
    val from_list : (name * value) list -> t
    val union : t -> t -> t
    val empty: t
    val support: t -> name list
    val partition: (name * value -> bool) -> t -> t * t
    val content: t -> (name * value) list
    val to_string: string -> t -> string
    val to_short_string: t -> string
    val print: t -> out_channel -> unit
    val mapi: (name -> value -> value) -> t -> t
    val iter: (name -> value -> unit) -> t -> unit
    val fold: (name -> value -> 'a -> 'a) -> t -> 'a -> 'a
  end

val eq_num: num -> num -> bool
