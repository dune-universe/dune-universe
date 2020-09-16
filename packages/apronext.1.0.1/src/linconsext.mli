(** This file is an extension for the Lincons1 module from the apron
Library *)

(** Note : It only adds function, nothing is removed. Extensions are at
the end of the module *)

open Apron
open Lincons1

type t = Lincons1.t

(** Make a linear constraint. Modifying later the linear expression ({e not
  advisable}) modifies correspondingly the linear constraint and conversely,
  except for changes of environements *)
val make: Linexpr1.t -> typ -> t

(** Copy (deep copy) *)
val copy: t -> t

(** Convert a constraint type to a string ([=],[>=], or [>]) *)
val string_of_typ : typ -> string

(** Print the linear constraint *)
val print : Format.formatter -> t -> unit

(** Get the constraint type *)
val get_typ: t -> typ

(** Iter the function on the pair coefficient/variable of the underlying linear
  expression *)
val iter: (Coeff.t -> Var.t -> unit) -> t -> unit

(** Get the constant of the underlying linear expression *)
val get_cst: t -> Coeff.t

(** Set the constraint type *)
val set_typ: t -> typ -> unit

(** Set simultaneously a number of coefficients.

  [set_list expr [(c1,"x"); (c2,"y")] (Some cst)] assigns coefficients [c1]
  to variable ["x"], coefficient [c2] to variable ["y"], and coefficient [cst]
  to the constant. If [(Some cst)] is replaced by [None],
  the constant coefficient is not assigned. *)
val set_list : t -> (Coeff.t * Var.t) list -> Coeff.t option -> unit

(** Set simultaneously a number of coefficients, as [set_list]. *)
val set_array : t -> (Coeff.t * Var.t) array -> Coeff.t option -> unit

(** Set the constant of the underlying linear expression *)
val set_cst: t -> Coeff.t -> unit

(** Get the coefficient of the variable in the underlying linear expression *)
external get_coeff : t -> Var.t -> Coeff.t
  = "camlidl_lincons1_ap_lincons1_get_coeff"

(** Set the coefficient of the variable in the underlying linear expression *)
external set_coeff : t -> Var.t -> Coeff.t -> unit
  = "camlidl_lincons1_ap_lincons1_set_coeff"

(** Build the unsatisfiable constraint -1>=0 *)
external make_unsat : Environment.t -> t
  = "camlidl_lincons1_ap_lincons1_make_unsat"

(** Is the constraint not satisfiable ? *)
external is_unsat : t -> bool
  = "camlidl_lincons1_ap_lincons1_is_unsat"

(** Change the environement of the constraint for a super-environement. Raise [Failure] if it is not the case *)
external extend_environment : t -> Environment.t -> t
  = "camlidl_lincons1_ap_lincons1_extend_environment"

(** Side-effect version of the previous function *)
external extend_environment_with : t -> Environment.t -> unit
  = "camlidl_lincons1_ap_lincons1_extend_environment_with"


(** Get the environement of the linear constraint *)
val get_env: t -> Environment.t

(** Get the underlying linear expression. Modifying the linear expression ({e
  not advisable}) modifies correspondingly the linear constraint and
  conversely, except for changes of environements *)
val get_linexpr1: t -> Linexpr1.t

(** Get the underlying linear constraint of level 0. Modifying the constraint
  of level 0 ({e not advisable}) modifies correspondingly the linear constraint
  and conversely, except for changes of environements*)
val get_lincons0: t -> Lincons0.t


(* ====================================================================== *)
(** {3 Type array} *)
(* ====================================================================== *)

(** Make an array of linear constraints with the given size and defined on the
  given environement. The elements are initialized with the constraint 0=0. *)
val array_make : Environment.t -> int -> earray

(** Print an array of constraints *)
val array_print :
  ?first:(unit, Format.formatter, unit) format ->
  ?sep:(unit, Format.formatter, unit) format ->
  ?last:(unit, Format.formatter, unit) format ->
  Format.formatter -> earray -> unit

(** Get the size of the array *)
val array_length : earray -> int

(** Get the environment of the array *)
val array_get_env : earray -> Environment.t

(** Get the element of the given index (which is not a copy) *)
val array_get : earray -> int -> t

(** Set the element of the given index (without any copy). The array and the
  constraint should be defined on the same environement; otherwise a [Failure]
  exception is raised.*)
val array_set : earray -> int -> t -> unit

(** Change the environement of the array of constraints for a super-environement. Raise [Failure] if it is not the case*)
external array_extend_environment : earray -> Environment.t -> earray
  = "camlidl_lincons1_ap_lincons1_array_extend_environment"

(** Side-effect version of the previous function *)
external array_extend_environment_with : earray -> Environment.t -> unit
  = "camlidl_lincons1_ap_lincons1_array_extend_environment_with"


(* ====================================================================== *)
(** {3 Extensions} *)
(* ====================================================================== *)

(** type of constraint negation *)
val neg_typ : typ -> typ

(** constraints negation : constructs a new constraint in opposite
direction.  e.g : a >= b -> a < b *)
val neg : t-> t

(** Equality splitting : split a = b into a >= b and a <= b.
 Raises Invalid_argument if the constraint is not an equality *)
val spliteq : t -> t * t

(** Disequality splitting split a <> b into a > b or a < b,
 Raises Invalid_argument if the constraint is not a disequality *)
val splitdiseq : t -> t * t

(** Higher-order functions utilities *)

(** fold function over generator.earay *)
val array_fold : ('a ->t-> 'a) -> 'a -> earray -> 'a

(** iter function over generator.earay *)
val array_iter : (t -> unit) -> earray -> unit

(** forall function over generator.earay *)
val array_for_all : (t -> bool) -> earray -> bool

(** exists function over generator.earay *)
val array_exists : (t -> bool) -> earray -> bool

(** to list conversion *)
val array_to_list : earray ->t list

(** of list build *)
val array_of_list :t list -> earray
