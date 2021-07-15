(* Time-stamp: <modified the 25/02/2016 (at 17:24) by Erwan Jahier> *)

(** Pretty-printing the Syntax Tree *)


val packinfo : Format.formatter -> AstV6.pack_info Lxm.srcflagged -> unit
val packbody : Format.formatter -> AstV6.packbody -> unit

val t : Format.formatter -> AstV6.t -> unit



val modelinfo : Format.formatter -> AstV6.model_info Lxm.srcflagged -> unit
val op2string : AstCore.by_pos_op -> string


(**/**)
val print_val_exp : out_channel -> AstCore.val_exp -> unit
val print_short_val_exp : out_channel -> AstCore.val_exp -> unit
val print_node_exp : out_channel -> AstCore.node_exp -> unit


val dump_val_exp : Format.formatter -> AstCore.val_exp -> unit
val dump_type_exp : Format.formatter -> AstCore.type_exp -> unit
val dump_static_arg :  Format.formatter -> Lv6Id.t * AstCore.static_arg Lxm.srcflagged -> unit
