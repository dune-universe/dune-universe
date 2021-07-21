(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: rif.ml
** Author: erwan.jahier@univ-grenoble-alpes.fr
*)


(** RIF (Reactive Input Format) utilities *)


(** Reads the input values *)
val read : bool -> in_channel -> out_channel option -> Exp.var list -> Var.env_in
(* nb: [read] uses [read_vntl] *)


val write : out_channel -> string -> unit

(** [write_outputs oc  outputs ] writes the Lucky outputs *)
val write_outputs : out_channel -> Exp.var list -> Value.OfIdent.t -> unit

(** [write_interface oc in_vars_ out_vars out_vars loc_vars oracle_vars] writes the input
    and output var names and types *)
val write_interface : out_channel -> 
  Exp.var list -> Exp.var list -> Exp.var list option -> Exp.var list list option -> unit

val flush : out_channel -> unit
