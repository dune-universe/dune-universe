(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: exp.mli
** Author: erwan.jahier@univ-grenoble-alpes.fr
*)

(** Lucky Boolean and numeric expressions. *)

type ext_func_name = string
type ext_func_type = Type.t list
type ext_lib_name = string
type ext_func_tbl = (ext_func_type * (ext_lib_name * Ezdl.t)) Util.StringMap.t

type t =
    Formu of formula
  | Numer of num
  | Liste of simple_tbl
      (** struct and arrays are flattened in such a list *)

and formula =
  | And  of formula * formula
  | Or   of formula * formula
  | Xor  of formula * formula
  | NXor of formula list
  | Nor  of formula list
  | Diese of formula list
  | Impl of formula * formula
  | IteB of formula * formula * formula
  | Not  of formula
  | EqB  of formula * formula

  | True
  | False
  | Bvar of var

  | Eq    of num * num (* =  *)
  | Sup   of num * num (* >  *)
  | SupEq of num * num (* >= *)
  | Inf   of num * num (* <  *)
  | InfEq of num * num (* <= *)

and
  (* cf the [Var] module. *)
  var = t Var.t

and func_call_arg = string * Ezdl.cfunc * ext_func_type * ext_lib_name * t list

and num =
  | Sum  of num * num
  | Diff of num * num
  | Prod of num * num
  | Quot of num * num
  | Mod  of num * num  (* modulo *)
  | Div  of num * num  (* euclidian division *)

  | Uminus of num

  | Inf_int
  | Ival of Num.num
  | Fval of float
  | Ivar of var
  | Fvar of var

  | FFC of func_call_arg
  | IFC of func_call_arg
  | Gcont of num * num * num
  | Gstop of num * num * num
  | Icont of num * num * num
  | Istop of num * num * num

  | Ite of formula * num * num

and simple =
    Fo of formula
  | Nu of num

and simple_tbl
  = simple Util.StringMap.t
(* This table, used to represent the flatten list of atomic values
   of a structured type expression. It is indexed by the string 
   that would be used to access the corresponding element.

   For exemple, a variable of type {f1:int^3 ; f2:bool} will represented
   by a map indexed by the strings ".f1[0]", ".f1[1]", ".f1[2]", and ".f2".
*)


val ext_func_tbl_to_string : ext_func_tbl -> string

(** A weight can be an integer, or a integer expression that depends only
 on inputs and memories. *)
type weight =
    Wint of int
  | Wexpr of num
  | Infinity

val weight_to_string : weight -> string

(* Table of vars indexed by name *)
type var_tbl = var Util.StringMap.t

val add_value : string -> simple -> simple_tbl -> simple_tbl
val add_var : string -> var -> var_tbl -> var_tbl
val empty_var_tbl : var Util.StringMap.t
val empty_simple_tbl : simple Util.StringMap.t


(** remove from a string everything that is before the 
  first "." or the first "[". This is useful to go to
  var_tbl indexes (which depends on the var name) to
  simple_tbl indexes (which depend only on the type).
*)
val remove_prefix : string -> string -> string -> string


(**/**)

val support : formula -> Var.name list
(** [support f] returns the support of f, if it is a cube made of Boolean var *)


val num_is_an_int : num -> bool


(** [num_to_var_value e] translate the expression [e] into a value
  if possible, fails otherwise *)
val num_to_var_value : num -> Value.t

(** [formula_to_var_value f] translates the formula [f] into values
  if possible, fails otherwise. *)
val formula_to_var_value : formula -> Value.t

val to_value : t -> Value.t



(** fails if the result is a list *)
val to_simple : t -> simple


(****************************************************************************)
(** Pretty prints formula and expressions. *)

val formula_to_string : formula -> string
val num_to_string : num -> string


val to_string : t -> string
val print_var : var -> unit

val remove_var_name : string -> string
val simplifie_a_little : formula -> formula
(****************************************************************************)
val to_expr : formula -> Expr.t 
