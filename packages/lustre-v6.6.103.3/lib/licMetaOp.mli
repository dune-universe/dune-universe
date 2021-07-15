(* Time-stamp: <modified the 13/12/2012 (at 11:11) by Erwan Jahier> *)

(** Produces lic for predefined Meta operators  *)

(** Extract the node and the constant from a list of static args *)
val get_node_and_int_const: Lxm.t -> Lic.static_arg list -> Lic.node_key * int


(** Produces lic of a  predefined Meta operator  *)
val do_node : (Lic.node_key -> Lic.node_exp) -> Lic.node_key -> Lxm.t -> Lic.node_exp
