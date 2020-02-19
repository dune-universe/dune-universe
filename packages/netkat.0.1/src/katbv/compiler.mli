open Base

(** [of_bexp_katbb bexp] is the Boolean KAT+B! expression representing [bexp] *)
val to_bexp_katbb : Ast.bexp -> Katbb_lib.Ast.bexp

(** [of_exp_katbb exp] is the KAT+B! expression representing [exp] *)
val to_exp_katbb : Ast.exp -> Katbb_lib.Ast.exp

(** [to_bdd bexp ~mgr ~map_var] is the BDD representing [bexp] *)
val to_bdd : Ast.bexp -> mgr:Idds.Bdd.manager -> map_var:(string -> int)
  -> Idds.Bdd.t

(** [to_idd exp ~mgr ~map_var] is the IDD representing [exp] *)
val to_idd : Ast.exp -> mgr:Idds.Idd.manager -> map_var:(string -> int)
  -> Idds.Idd.t

(** [to_table exp] is the forwarding table representing [exp] *)
val to_table : Ast.exp -> Tables.t