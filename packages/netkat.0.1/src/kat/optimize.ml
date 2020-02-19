open Base
open Ast

let ctrue = True
let cfalse = False

let skip = Assert ctrue
let abort = Assert cfalse

let test t = Test t

let conj b1 b2 =
  match b1, b2 with
  | (False as b), _ | _, (False as b)
  | True, b | b, True ->
    b
  | _, _ ->
    if phys_equal b1 b2 then b1 else
    Conj (b1, b2)

let disj b1 b2 =
  match b1, b2 with
  | (True as b), _ | _, (True as b)
  | False, b | b, False ->
    b
  | _, _ ->
    if phys_equal b1 b2 then b1 else
    Disj (b1, b2)

let neg b =
  match b with
  | True -> cfalse
  | False -> ctrue
  | Neg b -> b
  | _ -> Neg b


let assrt b = Assert b
let action a = Action a

let union e1 e2 =
  match e1, e2 with
  | Assert False, b | b, Assert False ->
    b
  | _, _ ->
    if phys_equal e1 e2 then e1 else
    Union (e1, e2)

let seq e1 e2 =
  match e1, e2 with
  | (Assert False as e), _ | _, (Assert False as e)
  | Assert True, e | e, Assert True ->
    e
  | _, _ ->
    Seq (e1, e2)

let star e =
  match e with
  | Assert _ -> skip
  | Star _ -> e
  | _ -> Star e

let ite b e1 e2 =
  union (seq (assrt b) e1) (seq (assrt (neg b)) e2)

let big_disj = List.fold_left ~init:cfalse ~f:disj

let big_conj = List.fold_left ~init:ctrue ~f:conj

let big_union = List.fold_left ~init:abort ~f:union

let big_seq = List.fold_left ~init:skip ~f:seq

let optimize_bexp ?(negate=false) ?neg_test b =
  let neg_test = match neg_test with
    | Some f -> fun t -> Test (f t)
    | None -> fun t -> Neg (Test t)
  in
  let rec opt neg = function
    | True -> if neg then cfalse else ctrue
    | False -> if neg then ctrue else cfalse
    | Test t as b -> if neg then neg_test t else b
    | Conj (b1, b2) -> conj (opt neg b1) (opt neg b2)
    | Disj (b1, b2) -> disj (opt neg b1) (opt neg b2)
    | Neg b -> opt (not neg) b
  in
  opt negate b

let rec optimize_exp ?neg_test e =
  match e with
  | Assert b -> assrt (optimize_bexp ?neg_test b)
  | Action _ -> e
  | Union (e1, e2) -> union (optimize_exp e1) (optimize_exp e2)
  | Seq (e1, e2) -> seq (optimize_exp e1) (optimize_exp e2)
  | Star e -> star (optimize_exp e)



let rec exp2_to_expn (e : ('act,'test) exp) : ('act,'test) Nary.exp =
  match e with
  | Assert b -> Assert b
  | Action a -> Action a
  | Union (e1, e2) ->
    begin match exp2_to_expn e1, exp2_to_expn e2 with
    (* merge asserts wherever possible *)
    | Assert b1, Assert b2 ->
      Assert (disj b1 b2)
    | Assert b1, Union (Assert b2 :: es) ->
      Union (Assert (disj b1 b2) :: es)
    | Union _, _ ->
      invalid_arg "union: not right-associative!"
    | e1, Union es -> Union (e1::es)
    | e1, e2 -> Union [e1; e2]
    end
  | Seq (e1, e2) ->
    begin match exp2_to_expn e1, exp2_to_expn e2 with
    (* merge asserts wherever possible *)
    | Assert b1, Assert b2 ->
      Assert (conj b1 b2)
    | Assert b1, Seq (Assert b2 :: es) ->
      Seq (Assert (conj b1 b2) :: es)
    | Seq _, _ ->
      invalid_arg "seq: not right-associative!"
    | e1, Seq es -> Seq (e1::es)
    | e1, e2 -> Seq [e1; e2]
    end
  | Star e -> Star (exp2_to_expn e)


let rec expn_to_exp2 (e : ('act, 'test) Nary.exp) : ('act, 'test) exp =
  match e with
  | Assert b -> assrt b
  | Action a -> action a
  | Union es ->
    List.fold es ~init:abort ~f:(fun f e -> union f (expn_to_exp2 e))
  | Seq es ->
    List.fold es ~init:skip ~f:(fun f e -> seq f (expn_to_exp2 e))
  | Star e -> star (expn_to_exp2 e)

let normalize_rassoc_exp e =
  expn_to_exp2 (exp2_to_expn e)