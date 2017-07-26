open Ppx_core

module type S = sig

  val hash_fold_type : loc:Location.t -> core_type -> core_type
  val hash_fold_core_type : core_type -> expression

  val hash_type : loc:Location.t -> core_type -> core_type
  val hash_core_type : core_type -> expression

  val str_attributes : Attribute.packed list

  val str_type_decl
    :  loc:Location.t
    -> path:string
    -> rec_flag * type_declaration list
    -> structure

  val sig_type_decl
    :  loc:Location.t
    -> path:string
    -> rec_flag * type_declaration list
    -> signature

end
