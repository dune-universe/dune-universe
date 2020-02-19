open Idds

(** BDDs form a Boolean algebra. *)
let bdd_ba mgr : Bdd.t Kat.Hom.ba = {
    ctrue = Bdd.ctrue;
    cfalse = Bdd.cfalse;
    conj = Bdd.conj mgr;
    disj = Bdd.disj mgr;
    neg = Bdd.neg mgr;
  }

(** IDDs form a KAT *)
let idd_kat mgr : (Idd.t, Bdd.t) Kat.Hom.kat = {
    ba = bdd_ba (Idd.get_bdd_manager mgr);
    assrt = Idd.of_bdd;
    union = Idd.union mgr;
    seq = Idd.seq mgr;
    star = Idd.star mgr;
  }


(* [bexp] compilation is a Boolean homomorphism from epressions to BDDs *)
let compile_bexp ~mgr ~map_var bexp =
  Kat.Hom.map_bexp bexp ~ba:(bdd_ba mgr) ~map_test:(fun Ast.{var; value} ->
    Bdd.test mgr (Var.inp (map_var var)) value
  )

(* [exp] compilation is a KAT homomorphism from epressions to IDDs *)
let compile_exp ~mgr ~map_var exp =
  let bdd_mgr = Idd.get_bdd_manager mgr in
  Kat.Hom.map_exp exp 
    ~kat:(idd_kat mgr) 
    ~map_test:(fun Ast.{ var; value } ->
      Bdd.test bdd_mgr (Var.inp (map_var var)) value
    )
    ~map_act:(fun Ast.{ var; value } ->
      Idd.set mgr (map_var var) value
    )
