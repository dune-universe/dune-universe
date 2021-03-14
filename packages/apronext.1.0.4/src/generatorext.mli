(** This file is an extension for the Generator1 module from the apron Library *)

(** Note : It only adds function, nothing is removed. Extensions are at the end
    of the documentation *)

open Apron
open Generator1

type t = Generator1.t

val make : Linexpr1.t -> Generator0.typ -> t
(** Make a generator. Modifying later the linear expression ({e not advisable})
    modifies correspondingly the generator and conversely, except for changes of
    environements *)

val copy : t -> t
(** Copy (deep copy) *)

val print : Format.formatter -> t -> unit
(** Print the generator *)

val get_typ : t -> Generator0.typ
(** Get the generator type *)

val iter : (Coeff.t -> Var.t -> unit) -> t -> unit
(** Iter the function on the pair coefficient/variable of the underlying linear
    expression *)

val set_typ : t -> Generator0.typ -> unit
(** Set the generator type *)

val set_list : t -> (Coeff.t * Var.t) list -> unit
(** Set simultaneously a number of coefficients.

    [set_list expr \[(c1,"x"); (c2,"y")\]] assigns coefficients [c1] to variable
    ["x"] and coefficient [c2] to variable ["y"]. *)

val set_array : t -> (Coeff.t * Var.t) array -> unit
(** Set simultaneously a number of coefficients, as [set_list]. *)

external get_coeff : t -> Var.t -> Coeff.t
  = "camlidl_generator1_ap_generator1_get_coeff"
(** Get the coefficient of the variable in the underlying linear expression *)

external set_coeff : t -> Var.t -> Coeff.t -> unit
  = "camlidl_generator1_ap_generator1_set_coeff"
(** Set the coefficient of the variable in the underlying linear expression *)

external extend_environment : t -> Environment.t -> t
  = "camlidl_generator1_ap_generator1_extend_environment"
(** Change the environement of the generator for a super-environement. Raise
    [Failure] if it is not the case *)

external extend_environment_with : t -> Environment.t -> unit
  = "camlidl_generator1_ap_generator1_extend_environment_with"
(** Side-effect version of the previous function *)

(* ====================================================================== *)
(** {3 Type earray} *)
(* ====================================================================== *)

val array_make : Environment.t -> int -> earray
(** Make an array of generators with the given size and defined on the given
    environement. The elements are initialized with the line 0. *)

val array_print :
     ?first:(unit, Format.formatter, unit) format
  -> ?sep:(unit, Format.formatter, unit) format
  -> ?last:(unit, Format.formatter, unit) format
  -> Format.formatter
  -> earray
  -> unit
(** Print an array of generators *)

val array_length : earray -> int
(** Get the size of the array *)

val array_get : earray -> int -> t
(** Get the element of the given index (which is not a copy) *)

val array_set : earray -> int -> t -> unit
(** Set the element of the given index (without any copy). The array and the
    generator should be defined on the same environement; otherwise a [Failure]
    exception is raised.*)

external array_extend_environment : earray -> Environment.t -> earray
  = "camlidl_generator1_ap_generator1_array_extend_environment"
(** Change the environement of the array of generators for a super-environement.
    Raise [Failure] if it is not the case*)

external array_extend_environment_with : earray -> Environment.t -> unit
  = "camlidl_generator1_ap_generator1_array_extend_environment_with"
(** Side-effect version of the previous function *)

val get_env : t -> Environment.t
(** Get the environement of the generator *)

val get_linexpr1 : t -> Linexpr1.t
(** Get the underlying linear expression. Modifying the linear expression
    ({e not advisable}) modifies correspondingly the generator and conversely,
    except for changes of environements *)

val get_generator0 : t -> Generator0.t
(** Get the underlying generator of level 0. Modifying the generator of level 0
    ({e not advisable}) modifies correspondingly the generator and conversely,
    except for changes of environements*)

(* ====================================================================== *)
(** {3 Extensions} *)
(* ====================================================================== *)

val neg : t -> t
(** constructs a new generator in opposite direction *)

val to_float_array : earray -> float array array
(** Converts a Generator1 into a float array array. *)

val of_rational_point : Environment.t -> Mpqf.t list -> t
(** returns a generator corresponding to a rational point *)

val of_float_point : Environment.t -> float list -> t
(** returns a generator corresponding to a float point *)

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

val to_vertices2D : t -> Var.t -> Var.t -> float * float

val to_vertices2D_s : t -> string -> string -> float * float

val to_vertices3D : t -> Var.t -> Var.t -> Var.t -> float * float * float

val to_vertices3D_s : t -> string -> string -> string -> float * float * float
