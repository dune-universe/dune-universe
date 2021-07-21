(*


Everything concerning the guards during AutoExplore.

In particular, performs partial eval of CoAlgExp.t

Uses : 
- Var.env
- CoAlgExp

*)


type t

val empty : t

type store = { curs : Value.OfIdent.t; pres: Value.OfIdent.t }

type unalias = CoIdent.t ->  CoAlgExp.t

val empty_store : store

val get_store : Value.OfIdent.t -> Value.OfIdent.t -> store 

val to_string : t -> string

exception Unsat

val of_exp : ?unalias:unalias -> ?context:store option -> CoAlgExp.t -> CoTraceExp.src_info -> t

val add : ?unalias:unalias -> ?context:store option -> CoAlgExp.t -> t -> CoTraceExp.src_info -> t

val merge : t -> t -> t

val dumpf : out_channel -> t -> unit

val to_exp_list : t -> CoAlgExp.t list

(* e.g. eval curs pres e *)
val simplify_exp : unalias -> store -> CoAlgExp.t -> CoAlgExp.t 
(* val simplify     : store -> t -> t  *)

(* exp to value, Not_constant if not constant *)
exception Not_constant of CoAlgExp.t
val value_of_algexp : unalias -> store -> CoAlgExp.t -> Value.t 
