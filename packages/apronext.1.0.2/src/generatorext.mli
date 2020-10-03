(** This file is an extension for the Generator1 module from the apron
Library *)

(** Note : It only adds function, nothing is removed. Extensions are at
the end of the documentation *)

open Apron
open Generator1

type t = Generator1.t

(** Make a generator. Modifying later the linear expression ({e not
  advisable}) modifies correspondingly the generator and conversely,
  except for changes of environements *)
val make: Linexpr1.t -> Generator0.typ -> t

(** Copy (deep copy) *)
val copy: t-> t

(** Print the generator *)
val print : Format.formatter -> t -> unit

(** Get the generator type *)
val get_typ:t-> Generator0.typ

(** Iter the function on the pair coefficient/variable of the underlying linear
  expression *)
val iter: (Coeff.t -> Var.t -> unit) ->t-> unit

(** Set the generator type *)
val set_typ:t-> Generator0.typ -> unit

(** Set simultaneously a number of coefficients.

  [set_list expr [(c1,"x"); (c2,"y")]] assigns coefficients [c1]
  to variable ["x"] and coefficient [c2] to variable ["y"]. *)
val set_list :t-> (Coeff.t * Var.t) list -> unit

(** Set simultaneously a number of coefficients, as [set_list]. *)
val set_array :t-> (Coeff.t * Var.t) array -> unit


(** Get the coefficient of the variable in the underlying linear expression *)
external get_coeff :t-> Var.t -> Coeff.t
  = "camlidl_generator1_ap_generator1_get_coeff"

(** Set the coefficient of the variable in the underlying linear expression *)
external set_coeff :t-> Var.t -> Coeff.t -> unit
  = "camlidl_generator1_ap_generator1_set_coeff"

(** Change the environement of the generator for a super-environement. Raise [Failure] if it is not the case *)
external extend_environment :t-> Environment.t -> t
  = "camlidl_generator1_ap_generator1_extend_environment"

(** Side-effect version of the previous function *)
external extend_environment_with :t-> Environment.t -> unit
  = "camlidl_generator1_ap_generator1_extend_environment_with"

(* ====================================================================== *)
(** {3 Type earray} *)
(* ====================================================================== *)

(** Make an array of generators with the given size and defined on the
  given environement. The elements are initialized with the line 0. *)
val array_make : Environment.t -> int -> earray

(** Print an array of generators *)
val array_print :
  ?first:(unit, Format.formatter, unit) format ->
  ?sep:(unit, Format.formatter, unit) format ->
  ?last:(unit, Format.formatter, unit) format ->
  Format.formatter -> earray -> unit

(** Get the size of the array *)
val array_length : earray -> int

(** Get the element of the given index (which is not a copy) *)
val array_get : earray -> int -> t

(** Set the element of the given index (without any copy). The array and the
  generator should be defined on the same environement; otherwise a [Failure]
  exception is raised.*)
val array_set : earray -> int ->t-> unit

(** Change the environement of the array of generators for a super-environement. Raise [Failure] if it is not the case*)
external array_extend_environment : earray -> Environment.t -> earray
  = "camlidl_generator1_ap_generator1_array_extend_environment"

(** Side-effect version of the previous function *)
external array_extend_environment_with : earray -> Environment.t -> unit
  = "camlidl_generator1_ap_generator1_array_extend_environment_with"


(** Get the environement of the generator *)
val get_env:t-> Environment.t

(** Get the underlying linear expression. Modifying the linear expression ({e
  not advisable}) modifies correspondingly the generator and
  conversely, except for changes of environements *)
val get_linexpr1:t-> Linexpr1.t

(** Get the underlying generator of level 0. Modifying the generator
  of level 0 ({e not advisable}) modifies correspondingly the generator
  and conversely, except for changes of environements*)
val get_generator0:t-> Generator0.t


(* ====================================================================== *)
(** {3 Extensions} *)
(* ====================================================================== *)

(** constructs a new generator in opposite direction *)
val neg :t-> t

(** Converts a Generator1 into a float array array. *)
val to_float_array : earray -> float array array

(** returns a generator corresponding to a float point *)
val of_float_point : Environment.t -> float list -> t

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
val array_of_list : t list -> earray

val to_vertices2D : t -> Var.t -> Var.t -> float * float
val to_vertices2D_s : t -> string -> string -> float * float
val to_vertices3D : t -> Var.t -> Var.t -> Var.t -> float * float * float
val to_vertices3D_s : t -> string -> string -> string -> float * float * float
