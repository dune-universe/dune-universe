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



(** [read_interface label ic oc_opt] reads the IO names and types on ic. The string
read on ic should looks like :
  #inputs v1:t1 v2:t2 ...
  #outputs x1:tx1 ...
  #step
    
    [label] is a string used in debug mode to hint who is calling.

 *)
val read_interface : ?debug:(bool) -> ?label:(string) -> in_channel -> 
  out_channel option -> Data.vntl * Data.vntl

exception Bye
exception Reset

(** Reads the input values. raises Bye if a line equal to "q" is read. *)
val read :  ?debug:(bool) -> ?label:(string) -> ?pragma:(string list) -> in_channel -> 
  out_channel option -> Data.vntl -> Data.subst list

val write : out_channel -> string -> unit

(** [write_outputs oc float_to_string outputs ] writes the outputs *)
val write_outputs : out_channel -> (float -> string) -> Data.vntl -> Data.subst list -> unit

(** [write_interface oc in_vars_ out_vars loc_vars oracle_vars] writes the input
    and output var names and types *)
val write_interface : out_channel -> Data.vntl -> Data.vntl -> Data.vntl option -> 
  Data.vntl list option -> unit

val flush : out_channel -> unit
