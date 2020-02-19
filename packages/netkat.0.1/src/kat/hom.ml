type 'b ba = {
  ctrue : 'b;
  cfalse : 'b;
  conj : 'b -> 'b -> 'b;
  disj : 'b -> 'b -> 'b;
  neg : 'b -> 'b;
}

type ('k, 'b) kat = {
  ba : 'b ba;
  assrt : 'b -> 'k;
  union : 'k -> 'k -> 'k;
  seq : 'k -> 'k -> 'k;
  star : 'k -> 'k;
}

let map_bexp
  ~(ba : 'b ba)
  ~(map_test : 'test -> 'b)
  (bexp : 'test Ast.bexp) : 'b
=
  let rec interp : 'test Ast.bexp -> 'b = function
    | True -> ba.ctrue
    | False -> ba.cfalse
    | Test t -> map_test t
    | Conj (b1, b2) -> ba.conj (interp b1) (interp b2)
    | Disj (b1, b2) -> ba.disj (interp b1) (interp b2)
    | Neg b -> ba.neg (interp b)
  in
  interp bexp

let map_exp
  ~(kat : ('k, 'b) kat)
  ~(map_test : 'test -> 'b)
  ~(map_act : 'act -> 'k)
  (exp : ('act, 'test) Ast.exp) : 'k
=
  let rec interp : ('act,'test) Ast.exp -> 'k = function
    | Assert bexp -> kat.assrt (map_bexp ~ba:kat.ba ~map_test bexp)
    | Action act -> map_act act
    | Union (e1, e2) -> kat.union (interp e1) (interp e2)
    | Seq (e1, e2) -> kat.seq (interp e1) (interp e2)
    | Star e -> kat.star (interp e)
  in
  interp exp
