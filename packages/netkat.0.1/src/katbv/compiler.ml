open Base
open Ast

let ba : Katbb_lib.Ast.bexp Kat.Hom.ba = {
  ctrue = Kat.Ast.True;
  cfalse = Kat.Ast.False;
  conj = (fun a b -> Kat.Ast.Conj (a,b));
  disj = (fun a b -> Kat.Ast.Disj (a,b));
  neg = (fun a -> Kat.Ast.Neg a);
}

let kat : (Katbb_lib.Ast.exp, Katbb_lib.Ast.bexp) Kat.Hom.kat = {
  ba = ba;
  assrt = (fun a -> Kat.Ast.Assert a);
  union = (fun a b -> Kat.Ast.Union (a,b));
  seq = (fun a b -> Kat.Ast.Seq (a,b));
  star = (fun a -> Kat.Ast.Star a);
}

let test v i bl = 
  Katbb_lib.Ast.{var = v ^ Int.to_string i; value = bl}

let build_test v z n = 
  let h (bl:bool) (i:int) : Katbb_lib.Ast.bexp = 
    Kat.Ast.Test (test v i bl) in
  Bitstring.build_term_list v z n h
  |> Kat.Optimize.big_conj

let map_test = function
  | Interval (v, a, b) ->
    (Kat.Optimize.conj (Bitstring.lower_bound v a) (Bitstring.upper_bound v b))
  | Test (v, z, n) -> 
    (build_test v z n)

let map_act (v, z, n) = 
  let h (bl:bool) (i:int) : Katbb_lib.Ast.exp = Kat.Ast.Action (test v i bl) in
  Bitstring.build_term_list v z n h
  |> Kat.Optimize.big_seq

let to_bexp_katbb = Kat.Hom.map_bexp ~ba ~map_test

let to_exp_katbb = Kat.Hom.map_exp ~kat ~map_test ~map_act

let to_bdd bexp = Katbb_lib.Idd_compiler.compile_bexp (to_bexp_katbb bexp)

let to_idd exp = Katbb_lib.Idd_compiler.compile_exp (to_exp_katbb exp)

let to_table = 
  let mgr = Idds.Idd.manager () in
  let tbl: int Hashtbl.M(String).t = Hashtbl.create (module String) in
  let next = ref (-1) in
  let map_var var =
    Hashtbl.find_or_add tbl var ~default:(fun () ->
      Int.incr next;
      !next
    )
  in
  Fn.compose Tables.to_table (to_idd ~mgr ~map_var) 