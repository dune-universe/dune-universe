open Idds

(** {1 Compilation} *)

(** [compile_bexp ~mgr ~map_var bexp] is the BDD representing [bexp] where
    [map_var] maps test names to their respective DD variable indices;
    [map_var] is required to be injective (or else the result is undefined).
*)
val compile_bexp : mgr:Bdd.manager -> map_var:(string -> int)
  -> Ast.bexp -> Bdd.t

(** [compile_exp ~mgr ~map_var exp] is the IDD representing [exp] where
    [map_var] maps test names to their respective DD variable indices;
    [map_var] is required to be injective (or else the result is undefined).
*)
val compile_exp : mgr:Idd.manager -> map_var:(string -> int)
  -> Ast.exp -> Idd.t


(** {1 Algebraic structures used for compilation} *)

(** BDDs form a Boolean algebra. *)
val bdd_ba : Bdd.manager -> Bdd.t Kat.Hom.ba

(** IDDs form a KAT. *)
val idd_kat : Idd.manager -> (Idd.t, Bdd.t) Kat.Hom.kat
