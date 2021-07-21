(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: var.mli
** Author: erwan.jahier@univ-grenoble-alpes.fr
*)


(** Lucky variables. *)

(** Abstract type encoding lucky variables and containing all
  informations attached to them.

  This variable type is parametrised by the type of expression
  variables refer to.
*)
type 'a t
(* The genericity has been introduced mainly to break the module recursivity
   with module [Exp]. *)


(** {4 Name, mode, and type of a variable.}  *)

type name = string
type mode = Input | Output | Local | Pre


(** {4 Substitutions.}  *)

(* map name -> Value.t *)
(*
module Name2Val :
  sig
    type t
    val get : t -> name -> Value.t
    val add : t -> (name * Value.t) -> t
    val add_list : t -> (name * Value.t) list -> t
    val from_list : (name * Value.t) list -> t
    val union : t -> t -> t
    val empty: t
    val support: t -> name list
    val partition: (name * Value.t -> bool) -> t -> t * t
    val content: t -> (name * Value.t) list
    val to_string: string -> t -> string
    val print: t -> out_channel -> unit
    val mapi: (name -> Value.t -> Value.t) -> t -> t
    val iter: (name -> Value.t -> unit) -> t -> unit
  end
*)

type subst = (name * Value.t)
type num_subst = (name * Value.num)


(** {4 Building Variables.} *)

(** Makes a var with the mandatory fields, i.e., its name, type, and mode. *)
(* [make lv_prefix var_name t m] *)
val make : string  -> string -> Type.t -> mode -> 'a t

(** Makes a pre var of a var.

    nb : "make_pre v" will be different from "make_pre v" (in the
    sense of ocaml compare) although they will have the same value in
    the end. It migth be useful to tabulate them in the caller.
*)
val make_pre : 'a t -> 'a t

(** {4 Sets the non-mandatory fields of a variable.} *)

val set_min   : 'a t -> 'a -> 'a t
val set_max   : 'a t -> 'a -> 'a t
val set_alias : 'a t -> 'a -> 'a t
val set_init  : 'a t -> 'a -> 'a t


(** {4 Variable information retrieval.}  Respectively retrieves the
name, type, mode, min, max, alias, and init values of a variable. Non
mandatory fields (of course) return an option type. *)

val name  : 'a t -> name
val typ   : 'a t -> Type.t
val mode  : 'a t -> mode

val min   : 'a t -> 'a option
val max   : 'a t -> 'a option
val alias : 'a t -> 'a option
val init  : 'a t -> 'a option


(** {4  Representing the input, output, and local variables vectors.} *)

(** We use an hash table to represent it as we will need to retrieve
 input values very often when evaluating formulas (and also weights).
 -> Replaced by a map *)

type env_in  = Value.OfIdent.t
(* type env_out = subst list *)
(* type env_loc = subst list *)
type env_out = Value.OfIdent.t
type env_loc = Value.OfIdent.t

(* generic env, no specially in/out or local...
   use instead of Value.OfIdent.t everywher Var.in/out/local
   is already used
*)
type env  = Value.OfIdent.t

(** For those types we use lists because the only operation we will
 need to perform over them is to add elements. *)




(****************************************************************************)
(****************************************************************************)

(**/**)

(** Access to the var instanciations *)
val get_val_env_in : env_in -> name -> Value.t
val get_val_env_out : env_out -> name -> Value.t
val get_val_env_loc : env_loc -> name -> Value.t


val print_env_in : env_in -> out_channel -> unit
val print_env_out : env_out -> out_channel -> unit
val print_env_loc : env_loc -> out_channel -> unit



val mode_of_string : string -> mode

(** A short-hand which stands for "variable name and type" introduced
  because it is used very often. *)
type vnt = name * Type.t


(****************************************************************************)
(* Do not document this one for the time being.  *)
val default : 'a t -> 'a option
val set_default : 'a t -> 'a -> 'a t
val change_type : 'a t -> Type.t -> 'a t

(* [is_newer var1 var2] is positive iff var1 has been created before var2 *)
val is_newer : 'a t -> 'a t -> int

(****************************************************************************)
(** Pretty prints. *)

val mode_to_string : mode -> string

(** [subst_list_to_string prefix sl] *)
val subst_list_to_string : string -> subst list -> string
val print_subst_list : subst list -> out_channel -> unit

val init_env_out : unit -> env_out
val init_env_loc : unit -> env_loc
val init_env_in : unit -> env_in

(* OBSOLETE
val inputs_to_list : env_in -> subst list
*)
(* full info print, requires 'a (that is, normally exp) printer  *)
val to_string_verbose : ('a -> string) -> 'a t -> string

val to_string : 'a t -> string
val print : 'a t -> unit


(**/**)
val index: 'a t -> int
