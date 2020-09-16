(** This file is an extension for the Tcons1 module from the apron
Library *)

(** Note : It only adds function, nothing is removed. Extensions are at
the end of the module *)

open Apron
open Tcons1

type t = Tcons1.t

type typ = Lincons0.typ =
|	EQ
|	SUPEQ
|	SUP
|	DISEQ
|	EQMOD of Scalar.t

(** Make a tree expression constraint. Modifying later the linear expression ({e not
  advisable}) modifies correspondingly the tree expression constraint and conversely,
  except for changes of environements *)
val make: Texpr1.t -> typ -> t

(** Copy (deep copy) *)
val copy: t -> t

(** Convert a constraint type to a string ([=],[>=], or [>]) *)
val string_of_typ : typ -> string

(** Print the tree expression constraint *)
val print : Format.formatter -> t -> unit

(** Get the constraint type *)
val get_typ: t -> typ

(** Set the constraint type *)
val set_typ: t -> typ -> unit

(** Change the environement of the constraint for a super-environement. Raise [Failure] if it is not the case *)
external extend_environment : t -> Environment.t -> t
  = "camlidl_tcons1_ap_tcons1_extend_environment"

(** Side-effect version of the previous function *)
external extend_environment_with : t -> Environment.t -> unit
  = "camlidl_tcons1_ap_tcons1_extend_environment_with"


(** Get the environement of the tree expression constraint *)
val get_env: t -> Environment.t

(** Get the underlying linear expression. Modifying the linear expression ({e
  not advisable}) modifies correspondingly the tree expression constraint and
  conversely, except for changes of environements *)
val get_texpr1: t -> Texpr1.t

(** Get the underlying tree expression constraint of level 0. Modifying the constraint
  of level 0 ({e not advisable}) modifies correspondingly the tree expression constraint
  and conversely, except for changes of environements*)
val get_tcons0: t -> Tcons0.t


(* ====================================================================== *)
(** {3 Type array} *)
(* ====================================================================== *)

(** Make an array of tree expression constraints with the given size and defined on the
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
  = "camlidl_tcons1_ap_tcons1_array_extend_environment"

(** Side-effect version of the previous function *)
external array_extend_environment_with : earray -> Environment.t -> unit
  = "camlidl_tcons1_ap_tcons1_array_extend_environment_with"


(* ====================================================================== *)
(** {3 Extensions} *)
(* ====================================================================== *)

(** The following functions build the constraints corresponding to 'e1
   op e2', where op is repectivelly (=,<>,>=,<=,>,<).  The type
   underlying expression will be [Real] if left unspecified, and its
   rounding mode will be [Near] if left unspecified *)
val eq : ?typ:Texpr1.typ -> ?round:Texpr1.round -> Texpr1.t -> Texpr1.t -> t
val diseq : ?typ:Texpr1.typ -> ?round:Texpr1.round -> Texpr1.t -> Texpr1.t -> t
val geq : ?typ:Texpr1.typ -> ?round:Texpr1.round -> Texpr1.t -> Texpr1.t -> t
val leq : ?typ:Texpr1.typ -> ?round:Texpr1.round -> Texpr1.t -> Texpr1.t -> t
val gt : ?typ:Texpr1.typ -> ?round:Texpr1.round -> Texpr1.t -> Texpr1.t -> t
val lt : ?typ:Texpr1.typ -> ?round:Texpr1.round -> Texpr1.t -> Texpr1.t -> t

(** type of constraint negation; i.e : EQ -> DISEQ | SUP -> SUPEQ *)
val neg_typ : typ -> typ

(** constraints negation; e.g : a >= b -> a < b *)
val neg : t -> t

(** split [a = b] into [(a > b),(a < b)] *)
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
