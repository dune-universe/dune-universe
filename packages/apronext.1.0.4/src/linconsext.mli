(** This file is an extension for the Lincons1 module from the apron Library *)

(** Note : It only adds function, nothing is removed. Extensions are at the end
    of the module *)

open Apron
open Lincons1

type t = Lincons1.t

val make : Linexpr1.t -> typ -> t
(** Make a linear constraint. Modifying later the linear expression
    ({e not advisable}) modifies correspondingly the linear constraint and
    conversely, except for changes of environements *)

val copy : t -> t
(** Copy (deep copy) *)

val string_of_typ : typ -> string
(** Convert a constraint type to a string ([=],[>=], or [>]) *)

val print : Format.formatter -> t -> unit
(** Print the linear constraint *)

val get_typ : t -> typ
(** Get the constraint type *)

val iter : (Coeff.t -> Var.t -> unit) -> t -> unit
(** Iter the function on the pair coefficient/variable of the underlying linear
    expression *)

val get_cst : t -> Coeff.t
(** Get the constant of the underlying linear expression *)

val set_typ : t -> typ -> unit
(** Set the constraint type *)

val set_list : t -> (Coeff.t * Var.t) list -> Coeff.t option -> unit
(** Set simultaneously a number of coefficients.

    [set_list expr \[(c1,"x"); (c2,"y")\] (Some cst)] assigns coefficients [c1]
    to variable ["x"], coefficient [c2] to variable ["y"], and coefficient [cst]
    to the constant. If [(Some cst)] is replaced by [None], the constant
    coefficient is not assigned. *)

val set_array : t -> (Coeff.t * Var.t) array -> Coeff.t option -> unit
(** Set simultaneously a number of coefficients, as [set_list]. *)

val set_cst : t -> Coeff.t -> unit
(** Set the constant of the underlying linear expression *)

external get_coeff : t -> Var.t -> Coeff.t
  = "camlidl_lincons1_ap_lincons1_get_coeff"
(** Get the coefficient of the variable in the underlying linear expression *)

external set_coeff : t -> Var.t -> Coeff.t -> unit
  = "camlidl_lincons1_ap_lincons1_set_coeff"
(** Set the coefficient of the variable in the underlying linear expression *)

external make_unsat : Environment.t -> t
  = "camlidl_lincons1_ap_lincons1_make_unsat"
(** Build the unsatisfiable constraint -1>=0 *)

external is_unsat : t -> bool = "camlidl_lincons1_ap_lincons1_is_unsat"
(** Is the constraint not satisfiable ? *)

external extend_environment : t -> Environment.t -> t
  = "camlidl_lincons1_ap_lincons1_extend_environment"
(** Change the environement of the constraint for a super-environement. Raise
    [Failure] if it is not the case *)

external extend_environment_with : t -> Environment.t -> unit
  = "camlidl_lincons1_ap_lincons1_extend_environment_with"
(** Side-effect version of the previous function *)

val get_env : t -> Environment.t
(** Get the environement of the linear constraint *)

val get_linexpr1 : t -> Linexpr1.t
(** Get the underlying linear expression. Modifying the linear expression
    ({e not advisable}) modifies correspondingly the linear constraint and
    conversely, except for changes of environements *)

val get_lincons0 : t -> Lincons0.t
(** Get the underlying linear constraint of level 0. Modifying the constraint of
    level 0 ({e not advisable}) modifies correspondingly the linear constraint
    and conversely, except for changes of environements*)

(* ====================================================================== *)
(** {3 Type array} *)
(* ====================================================================== *)

val array_make : Environment.t -> int -> earray
(** Make an array of linear constraints with the given size and defined on the
    given environement. The elements are initialized with the constraint 0=0. *)

val array_print :
     ?first:(unit, Format.formatter, unit) format
  -> ?sep:(unit, Format.formatter, unit) format
  -> ?last:(unit, Format.formatter, unit) format
  -> Format.formatter
  -> earray
  -> unit
(** Print an array of constraints *)

val array_length : earray -> int
(** Get the size of the array *)

val array_get_env : earray -> Environment.t
(** Get the environment of the array *)

val array_get : earray -> int -> t
(** Get the element of the given index (which is not a copy) *)

val array_set : earray -> int -> t -> unit
(** Set the element of the given index (without any copy). The array and the
    constraint should be defined on the same environement; otherwise a [Failure]
    exception is raised.*)

external array_extend_environment : earray -> Environment.t -> earray
  = "camlidl_lincons1_ap_lincons1_array_extend_environment"
(** Change the environement of the array of constraints for a
    super-environement. Raise [Failure] if it is not the case*)

external array_extend_environment_with : earray -> Environment.t -> unit
  = "camlidl_lincons1_ap_lincons1_array_extend_environment_with"
(** Side-effect version of the previous function *)

(* ====================================================================== *)
(** {3 Extensions} *)
(* ====================================================================== *)

val is_strict : t -> bool
(** true if the type is 'SUP' or 'DISEQ' constraint *)

val neg_typ : typ -> typ
(** type of constraint negation *)

val neg : t -> t
(** constraints negation : constructs a new constraint in opposite direction.
    e.g : a >= b -> a < b *)

val spliteq : t -> t * t
(** Equality splitting : split a = b into a >= b and a <= b. Raises
    Invalid_argument if the constraint is not an equality *)

val splitdiseq : t -> t * t
(** Disequality splitting split a <> b into a > b or a < b, Raises
    Invalid_argument if the constraint is not a disequality *)

val pp_print : Format.formatter -> t -> unit
(** Human readable pretty-printing *)

(** Higher-order functions utilities *)

val fold : (Coeff.t -> Var.t -> 'a -> 'a) -> (Coeff.t -> 'a) -> t -> 'a
(** [fold f g l] folds over the different monom of the linear expression [l],
    with as an initial accumulato the value [(g (get_cst l))]. *)

val array_fold : ('a -> t -> 'a) -> 'a -> earray -> 'a
(** fold function over generator.earay *)

val array_iter : (t -> unit) -> earray -> unit
(** iter function over generator.earay *)

val array_for_all : (t -> bool) -> earray -> bool
(** forall function over generator.earay *)

val array_exists : (t -> bool) -> earray -> bool
(** exists function over generator.earay *)

val array_to_list : earray -> t list
(** to list conversion *)

val array_of_list : t list -> earray
(** of list build *)
