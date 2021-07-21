(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: prevar.mli
** Author: erwan.jahier@univ-grenoble-alpes.fr
*)

(** Internal representation of pre variables. *)


(** To pretty print pre var. Leave the string unchanged if it is not a pre var *)
val format : string -> string

(** [is_pre_va vn] returns iff [vn] is a pre variable. *)
val is_pre_var : string -> bool

(** get the root name of the variable the pre holds on. 
    e.g, get_var_names "$p1x" returns "x", and "$p2x" returns "x" too.
*)
val get_root_var_name : string -> string

(** get the name of the variable the pre holds on. 
    e.g, get_pre_var_names "$p1x" returns "x", and "$p2x" returns "$p1x"
  *)
val get_pre_var_name : string -> string



(** [give_pre_var_name u] returns the string encoding of pre u*)
val give_pre_var_name : string -> string
