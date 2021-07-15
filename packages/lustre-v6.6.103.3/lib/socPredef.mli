(* Time-stamp: <modified the 06/09/2017 (at 16:32) by Erwan Jahier> *)

(** Synchronous Object Code for Predefined operators. *)


val of_soc_key : Lxm.t -> Soc.key -> Soc.t

(** Associe un opérateur Lustre et le type de ses opérandes à un SOC
    et sa fonction de typage.

    Le type des opérandes permet de traiter les opérateurs surchargés. 
*)
val soc_interface_of_pos_op: 
    Lxm.t -> Lic.by_pos_op -> Data.t list -> Soc.var_expr option -> Soc.t

val get_mem_name : Soc.key -> Data.t -> string
