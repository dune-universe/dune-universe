(** This file is an extension for the Tcons1 module from the apron Library *)

(** Note : It only adds function, nothing is removed. Extensions are at the end
    of the module *)

open Apron
open Tcons1

type t = Tcons1.t

type typ = Lincons0.typ = EQ | SUPEQ | SUP | DISEQ | EQMOD of Scalar.t

val make : Texpr1.t -> typ -> t
(** Make a tree expression constraint. Modifying later the linear expression
    ({e not advisable}) modifies correspondingly the tree expression constraint
    and conversely, except for changes of environements *)

val copy : t -> t
(** Copy (deep copy) *)

val string_of_typ : typ -> string
(** Convert a constraint type to a string ([=],[>=], or [>]) *)

val print : Format.formatter -> t -> unit
(** Print the tree expression constraint *)

val get_typ : t -> typ
(** Get the constraint type *)

val set_typ : t -> typ -> unit
(** Set the constraint type *)

external extend_environment : t -> Environment.t -> t
  = "camlidl_tcons1_ap_tcons1_extend_environment"
(** Change the environement of the constraint for a super-environement. Raise
    [Failure] if it is not the case *)

external extend_environment_with : t -> Environment.t -> unit
  = "camlidl_tcons1_ap_tcons1_extend_environment_with"
(** Side-effect version of the previous function *)

val get_env : t -> Environment.t
(** Get the environement of the tree expression constraint *)

val get_texpr1 : t -> Texpr1.t
(** Get the underlying linear expression. Modifying the linear expression
    ({e not advisable}) modifies correspondingly the tree expression constraint
    and conversely, except for changes of environements *)

val get_tcons0 : t -> Tcons0.t
(** Get the underlying tree expression constraint of level 0. Modifying the
    constraint of level 0 ({e not advisable}) modifies correspondingly the tree
    expression constraint and conversely, except for changes of environements*)

(* ====================================================================== *)
(** {3 Type array} *)
(* ====================================================================== *)

val array_make : Environment.t -> int -> earray
(** Make an array of tree expression constraints with the given size and defined
    on the given environement. The elements are initialized with the constraint
    0=0. *)

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
  = "camlidl_tcons1_ap_tcons1_array_extend_environment"
(** Change the environement of the array of constraints for a
    super-environement. Raise [Failure] if it is not the case*)

external array_extend_environment_with : earray -> Environment.t -> unit
  = "camlidl_tcons1_ap_tcons1_array_extend_environment_with"
(** Side-effect version of the previous function *)

(* ====================================================================== *)
(** {3 Extensions} *)
(* ====================================================================== *)

val eq : ?typ:Texpr1.typ -> ?round:Texpr1.round -> Texpr1.t -> Texpr1.t -> t
(** The following functions build the constraints corresponding to 'e1 op e2',
    where op is repectivelly (=,<>,>=,<=,>,<). The type underlying expression
    will be [Real] if left unspecified, and its rounding mode will be [Near] if
    left unspecified *)

val diseq : ?typ:Texpr1.typ -> ?round:Texpr1.round -> Texpr1.t -> Texpr1.t -> t

val geq : ?typ:Texpr1.typ -> ?round:Texpr1.round -> Texpr1.t -> Texpr1.t -> t

val leq : ?typ:Texpr1.typ -> ?round:Texpr1.round -> Texpr1.t -> Texpr1.t -> t

val gt : ?typ:Texpr1.typ -> ?round:Texpr1.round -> Texpr1.t -> Texpr1.t -> t

val lt : ?typ:Texpr1.typ -> ?round:Texpr1.round -> Texpr1.t -> Texpr1.t -> t

val neg : t -> t
(** constraints negation; e.g : a >= b -> a < b *)

val splitdiseq : t -> t * t
(** split [a = b] into [(a > b),(a < b)] *)

(** Higher-order functions utilities *)

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
