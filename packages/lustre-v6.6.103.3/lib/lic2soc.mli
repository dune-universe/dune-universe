(** Time-stamp: <modified the 09/09/2015 (at 11:23) by Erwan Jahier> *)


val f: LicPrg.t -> Lic.node_key -> Soc.key * Soc.tbl

val user_var_prefix:string

exception Polymorphic

(* raises the Polymorphic exception if Lic.type_ is polymorphic *)
val lic_to_data_type: Lic.type_ -> Data.t

val soc_profile_of_node: Lic.node_exp -> Soc.var list * Soc.var list
