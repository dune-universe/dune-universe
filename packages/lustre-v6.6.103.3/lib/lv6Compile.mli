(* Time-stamp: <modified the 26/02/2015 (at 13:45) by Erwan Jahier> *)

(** Main bis *)

(** [doit pl main_node] compiles the pack and model list using [main_node] as
    main node. *)


val doit : Lv6MainArgs.t -> AstV6.pack_or_model list -> Lv6Id.idref option -> LicPrg.t


val get_source_list : Lv6MainArgs.t -> string list -> AstV6.pack_or_model list
