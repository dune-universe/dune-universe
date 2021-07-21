open Expr
open Constraint

let (index_to_exp : Bddd.t -> int -> Expr.t * bool) =
  fun bddt i ->
    let aux ne = 
      [Ne.to_expr ne; if Ne.is_int ne then Ival (Num.Int 0) else Fval 0.0]
    in
    match Bddd.index_to_linear_constraint bddt i with
      | Bv(v)   -> Var (Var.name v), false
      | EqZ(ne) -> Op (Eq, aux ne), true
      | Ineq(GZ(ne))   -> Op (Sup, aux ne), true
      | Ineq(GeqZ(ne)) -> Op (SupEq, aux ne), true

let impl a b = not a || b

let (collect : Bddd.t -> bool -> Bdd.t -> Expr.t) =
  fun bddt num bdd ->
    (* Collect the set of paths leading to true.  If the [num] flag is
       true, collect only the numeric constraints.  *)
    let rec (aux: Bdd.t -> Expr.t list list) =
      (* collect the disjunction of conjunction as a list of list *)
      fun bdd ->
        assert(not(Bdd.is_true bdd));
        assert(not(Bdd.is_false bdd));
        let t,e = Bdd.dthen bdd, Bdd.delse bdd in
        let v,is_num = index_to_exp bddt (Bdd.topvar bdd) in
        let not_v = Op(Not,[v]) in
        let res_t =
          if (Bdd.is_true t) then (if (impl num is_num) then [[v]] else []) else 
            if (Bdd.is_false t) then [] else 
              List.map (fun x -> (if (impl num is_num) then v::x else x))  (aux t)
        in
        let res_e =
          if (Bdd.is_true e) then (if (impl num is_num) then [[not_v]] else []) else
            if (Bdd.is_false e) then [] else 
              List.map (fun x ->  (if (impl num is_num) then not_v::x else x))  (aux e)
        in
        List.rev_append res_e res_t
    in
    let res =
      match aux bdd with
        | [] -> assert false
        | [[e]] -> e
        | [l] -> Op(And, l)
        | ll -> Op(Or, List.map (fun l -> Op(And, l)) ll)
    in
    Expr.simplify res

(* substracting list *)
let lminus l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

(* exported *)
let (get_info : Bddd.t -> Bdd.t -> Bdd.t -> (Expr.t * Bdd.t) -> Expr.t) =
  fun bddt bdd bdd1 (expr2,bdd2) ->
    assert (not(Bdd.is_false bdd1));
    let is_sat = not (Bdd.is_false bdd) in
    if is_sat then collect bddt true bdd
    else if Bdd.is_false bdd2 then expr2 else
      (* try to simplify the formula associated to bdd by projet *)
      let s1 = Bdd.list_of_support (Bdd.support bdd1)
      and s2 = Bdd.list_of_support (Bdd.support bdd2) in
      let non_contributing_var = List.rev_append (lminus s1 s2)(lminus s2 s1) in
      let non_contributing_var = Bdd.support_of_list non_contributing_var in
      let bdd1 = Bdd.exist_local non_contributing_var bdd1
      and bdd2 = Bdd.exist_local non_contributing_var bdd2 in
      assert (not(Bdd.is_false bdd1));
      assert (not(Bdd.is_false bdd2));
      assert (Bdd.is_false (Bdd.dand bdd1 bdd2));        
      Expr.Op(Expr.And, [collect bddt false bdd1; collect bddt false bdd2])


