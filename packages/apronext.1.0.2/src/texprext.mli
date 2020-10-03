open Apron

(* This file is part of the APRON Library, released under LGPL license
   with an exception allowing the redistribution of statically linked
   executables.  Please read the COPYING file packaged in the
   distribution *)
(* File generated from texpr1.idl *)

type t = Texpr1.t = {
  	mutable texpr0 : Texpr0.t;
  	mutable env : Environment.t;
}

(** APRON Expressions of level 1 *)

(** Unary operators *)
type unop = Texpr0.unop =
  | Neg
  | Cast
  | Sqrt

(** Binary operators *)
type binop = Texpr0.binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Pow

(** Destination type for rounding *)
type typ = Texpr0.typ =
  | Real
  | Int
  | Single
  | Double
  | Extended
  | Quad

(** Rounding direction *)
type round = Texpr0.round =
  | Near
  | Zero
  | Up
  | Down
  | Rnd

(** User type for tree expressions *)
type expr = Texpr1.expr =
|	Cst of Coeff.t
|	Var of Var.t
|	Unop of unop * expr * typ * round
|	Binop of binop * expr * expr * typ * round


(** {3 Constructors and Destructor} *)

(** General constructor (actually the most efficient) *)
val of_expr : Environment.t -> expr -> t

(** Copy *)
val copy : t -> t

(** Conversion *)
val of_linexpr : Linexpr1.t -> t

(** General destructor *)
val to_expr : t -> expr


(** {4 Incremental constructors} *)

external cst : Environment.t -> Coeff.t -> t
  = "camlidl_texpr1_ap_texpr1_cst"

val var : Environment.t -> Var.t -> t
external unop : Texpr0.unop -> t -> Texpr0.typ -> Texpr0.round -> t
  = "camlidl_texpr1_ap_texpr1_unop"

external binop : Texpr0.binop -> t -> t -> Texpr0.typ -> Texpr0.round -> t
  = "camlidl_texpr1_ap_texpr1_binop"


(** {3 Tests} *)

val is_interval_cst : t -> bool
val is_interval_linear : t -> bool
val is_interval_polynomial : t -> bool
val is_interval_polyfrac : t -> bool
val is_scalar : t -> bool


(** {3 Operations} *)

(** Change the environment of the expression for a super-environment. Raise [Failure] if it is not the case *)
external extend_environment : t -> Environment.t -> t
  = "camlidl_texpr1_ap_texpr1_extend_environment"

(** Side-effet version of the previous function *)
external extend_environment_with : t -> Environment.t -> unit
  = "camlidl_texpr1_ap_texpr1_extend_environment_with"


(** Get the underlying expression of level 0 (which is not a copy). *)
val get_texpr0: t -> Texpr0.t

(** Get the environment of the expression *)
val get_env: t -> Environment.t



(** {3 Printing} *)


val string_of_unop  : unop  -> string
val string_of_binop : binop -> string
val string_of_typ   : typ   -> string
val string_of_round : round -> string
val print_unop  :  Format.formatter -> unop  -> unit
val print_binop :  Format.formatter -> binop -> unit
val print_typ   :  Format.formatter -> typ   -> unit
val print_round :  Format.formatter -> round -> unit

val print_expr : Format.formatter -> expr -> unit
(** Print a tree expression *)

val print : Format.formatter -> t -> unit
(** Print an abstract tree expression *)

(* ====================================================================== *)
(** {3 Extensions} *)
(* ====================================================================== *)

val cst_f : float -> expr
val var_s : string -> expr

val unary : ?typ:typ -> ?round:round -> unop -> t -> t
val neg : ?typ:typ -> ?round:round -> t -> t
val cast : ?typ:typ -> ?round:round -> t -> t
val sqrt : ?typ:typ -> ?round:round -> t -> t

val binary : ?typ:typ -> ?round:round -> binop -> t -> t -> t
val sub : ?typ:typ -> ?round:round -> t -> t -> t
val add : ?typ:typ -> ?round:round -> t -> t -> t
val mul : ?typ:typ -> ?round:round -> t -> t -> t
val div : ?typ:typ -> ?round:round -> t -> t -> t
val pow : ?typ:typ -> ?round:round -> t -> t -> t
val modulo : ?typ:typ -> ?round:round -> t -> t -> t
