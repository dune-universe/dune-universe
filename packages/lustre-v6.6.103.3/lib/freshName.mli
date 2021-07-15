(* Time-stamp: <modified the 03/03/2015 (at 14:29) by Erwan Jahier> *)


(** All new identifier names ougth to be created via this module. *)

(** To be called just after the parsing (to make sure that fresh
    var names won't clash with user idents. ) 

    Indeed, during the parsing, we collect the set of forbidden id that
    we (somehow) transmit to the fresh id generator below.
*)
val update_fresh_var_prefix : unit -> unit

(** [node_key nk] returns a fresh node ident. 

    The idea is the following: the caller propose a name to map the
    node key. But since that name may clash, we sometimes need to work
    a little bit more. This is the purpose of that function.
    
    nb : for a  node key, it will always return the same string 
    (which means that [name] migth be ignored).
*)
val node_key: Lic.node_key -> string -> string


(** Dealing with fresh local (to the node) variable idents *)

(** Returns a fresh local var name *)
val local_var : string -> string (* mv new_var? *)


(** Returns a fresh local var_info *)
val var_info : string -> Lic.type_ -> Lic.id_clock -> Lic.var_info

(**  *)
val array_type : Lic.type_ -> string -> string



