(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License 
**-----------------------------------------------------------------------
**
** File: bddd.ml
** Main author: erwan.jahier@univ-grenoble-alpes.fr
*)


(** In the following, we call a comb the bdd of a conjunction of
 litterals (var). They provide the ordering in which litterals
 appear in the bdds we manipulate.
*)

open Exp
open Value
open Constraint

type var_idx = int

(****************************************************************************)

open Sol_nb

(** snt Associates to a bdd the (absolute) number of solutions
  contained in its lhs (then) and rhs (else) branches. Note that
  adding those two numbers only give a number of solution that is
  relative to which bdd points to it. 
*)

module BddMap = struct
  include Map.Make(struct type t = Bdd.t let compare = compare end)
end

type snt = (sol_nb * sol_nb) BddMap.t
type t = snt * Formula_to_bdd.t 

let tbl_to_string (snt,f2b) =
  (BddMap.fold 
     (fun bdd (sn1, sn2) acc ->
        Printf.sprintf "%s; %i->%s,%s"  acc
          (Bdd.size bdd) (string_of_sol_nb sn1) (string_of_sol_nb sn2) 
     ) snt "|bdd|->sol_number_left, sol_number_rigth: " 
  ) ^ "\n" ^
  (Formula_to_bdd.tbl_to_string f2b)
exception No_numeric_solution	of t

let (clear : t -> t) =
  fun (snt, bddt)  ->
    snt, Formula_to_bdd.clear_step bddt

let (formula_to_bdd : t -> Var.env_in -> Var.env -> string -> int ->
     Exp.formula -> t * Bdd.t) =
  fun (snt,bddt) input memory ctx_msg vl f -> 
    let bddt, bdd = Formula_to_bdd.f bddt input memory ctx_msg vl f in
    (snt,bddt), bdd

let (index_to_linear_constraint : t -> int -> Constraint.t) =
  fun (_,bddt) i ->
    Formula_to_bdd.index_to_linear_constraint bddt i
      
let (get_index_from_linear_constraint : t -> Constraint.t -> int) =
  fun (_,bddt) c ->
    Formula_to_bdd.get_index_from_linear_constraint bddt c

let (num_to_gne: t -> Var.env_in -> Var.env -> string -> int -> Exp.num -> t * Gne.t) =
  fun (snt,bddt) i l s vl n ->
    let bddt, gne = Formula_to_bdd.num_to_gne bddt i l s vl n  in
    (snt,bddt), gne

let (eval_int_expr: t -> Exp.num -> string -> Var.env_in -> Var.env  -> int ->
     int option) =
  fun (_snt,bddt)  e msg input mem vl   -> 
  Formula_to_bdd.eval_int_expr bddt e msg input mem vl 


     
let (sol_number : t -> Bdd.t -> sol_nb * sol_nb) =
  fun (snt,_) bdd ->
    BddMap.find bdd snt 
 

let (add_snt_entry : t -> Bdd.t -> sol_nb * sol_nb -> t) =
  fun (snt,bddt) bdd sol_nb -> 
    BddMap.add bdd sol_nb snt, bddt

let (init_snt : unit -> t) = 
  fun () ->
    let snt = BddMap.empty in
    let f2bdd = Formula_to_bdd.init () in
    let bdd_true = Bdd.dtrue () in
    let bdd_false = Bdd.dfalse () in
    let snt = BddMap.add  (bdd_true) (one_sol, zero_sol) snt in
    let snt = BddMap.add  (bdd_false) (zero_sol, one_sol) snt in
    snt,f2bdd


let (sol_number_exists : Bdd.t -> snt -> bool) =
  fun bdd snt -> 
    BddMap.mem bdd snt


let rec (build_sol_nb_table_rec: Bdd.t -> Bdd.t -> t -> t * sol_nb) =
  fun bdd comb t -> 
    (* Returns the relative (to which bbd points to it) number of
        solutions of [bdd]. Also udpates the solution number table 
        for [bdd], and recursively for all its sub-bdds.

        [comb] is a positive cube that ougth to contain the indexes of
        boolean vars that are still to be generated, and the numerical
        indexes that appears in [bdd].  

        Note that this sol number make abstraction of the volume of
        polyhedron which dimension is greater than 2. It is very bad as
        far as the fairness of the draw is concerned, but really,
        computing it would be far too expensive for our purpose
        (real-time!). Cf fair_bddd.ml for a more fair version.
    *)
    let _ = assert (
      not (Bdd.is_cst bdd) && (Bdd.topvar comb) = (Bdd.topvar bdd) )
    in
    try
      (* either it has already been computed ... *)
      let (nt, ne) = sol_number t bdd in
      t, (add_sol_nb nt ne)

    with Not_found ->
      (* ... or not. *)
      let t, nt = compute_absolute_sol_nb (Bdd.dthen bdd) comb t in
      let (snt,bddt), ne = compute_absolute_sol_nb (Bdd.delse bdd) comb t in
      let snt = BddMap.add bdd (nt, ne) snt in
      (snt,bddt), (add_sol_nb nt ne)
and 
  (compute_absolute_sol_nb: Bdd.t  -> Bdd.t -> t -> t * sol_nb) =
  fun sub_bdd comb t -> 
    (*
      Returns the absolute number of solutions of [sub_bdd]  w.r.t. [comb], 
      where [comb] is the comb of the father of [sub_bdd].

      The [comb] is used to know which output Boolean variables are
      unconstraint along a path in the bdd. Indeed, the comb is made
      of all the Boolean output var indexes plus the num contraints
      indexes that appears in the bdd; hence, if the topvar of the
      bdd is different from the topvar of the comb, it means that
      the topvar of the comb is unsconstraint and we need to
      multiply the number of solution of the branch by 2.
    *)
    if Bdd.is_false sub_bdd then t, zero_sol
    else if Bdd.is_true sub_bdd then
      let sol_nb = if Bdd.is_true comb then one_sol else
          two_power_of (Bdd.supportsize (Bdd.dthen comb)) 
      in
      t, sol_nb
    else 
      let topvar = Bdd.topvar sub_bdd in
      let rec
        (count_missing_vars: Bdd.t -> var_idx -> int -> Bdd.t * int) =
        fun comb var_idx cpt -> 
          (* Returns [cpt] + the number of variables occurring in [comb]
             before reaching [var_idx] ([var_idx] excluded). Also returns the comb
             which topvar is [var_idx]. *)

          let _ = assert (not (Bdd.is_cst comb)) in
          let combvar = Bdd.topvar comb in
          if var_idx = combvar
          then (comb, cpt)
          else count_missing_vars (Bdd.dthen comb) var_idx (cpt+1)
      in
      let (sub_comb, missing_vars_nb) = count_missing_vars (Bdd.dthen comb) topvar 0 in
      let t, n0 = build_sol_nb_table_rec sub_bdd sub_comb t in
      let factor = (two_power_of missing_vars_nb) in
      let res = mult_sol_nb n0 factor in
      t, res

(** [build_sol_nb_table bdd comb] build an internal table that
  associates to each [bdd] its solution number. [comb] is a bdd made
  of the the conjunctions of variables of [bdd] union the Boolean
  variable to be generated (which migth not appear in [bdd]) ; it is 
  necessary because not all the variable to be generated appears
  in the [bdd].
*)   
   

let (build_sol_nb_table: t -> var list -> Bdd.t -> Bdd.t -> t) =
  fun t _ bdd comb ->
    if (sol_number_exists bdd (fst t)) then t else
      let rec skip_unconstraint_bool_var_at_top comb v =
        (* [build_sol_nb_table] supposes that the bdd and its comb have
           the same top var.  *)
        if Bdd.is_true comb then comb
        else
          let topvar = (Bdd.topvar comb) in
          if v = topvar then comb
          else skip_unconstraint_bool_var_at_top (Bdd.dthen comb) v
      in
      if Bdd.is_true bdd then
        fst (build_sol_nb_table_rec bdd comb t)
      else
        let tvar = Bdd.topvar bdd in
        let comb2 = skip_unconstraint_bool_var_at_top comb tvar in
        fst (build_sol_nb_table_rec bdd comb2 t)
        

(****************************************************************************)
(****************************************************************************)

let (toss_up_one_var: Var.name -> Var.subst) =
  fun var -> 
   (* *)
    let ran = Random.float 1. in
      if (ran < 0.5) then (var, B(true)) else (var, B(false))


let (toss_up_one_var_index:  Var.env_in -> Var.env -> int ->
     string -> Formula_to_bdd.t -> var_idx -> Formula_to_bdd.t * Var.subst option) =
  fun input memory vl ctx_msg bddt var_idx -> 
    (* if [var_idx] is a index that corresponds to a boolean variable,
       this fonction performs a toss and returns a substitution for
       the corresponding boolean variable. It returns [None]
       otherwise (ie if it is a num).

       Indeed, if it happens that a numerical constraint does not
       appear along a path, we simply ignore it and hence it will not
       be added to the store.
    *)
    let cstr = Formula_to_bdd.index_to_linear_constraint bddt var_idx in
    match cstr with
      Bv(v) -> (
        match (Var.default v) with
        | Some (Formu f) ->
          let bddt, bdd = Formula_to_bdd.f bddt input memory ctx_msg vl f in
          if Bdd.is_false bdd then bddt, Some((Var.name v), B false)
          else if Bdd.is_true bdd then bddt, Some((Var.name v), B true)
          else
            (print_string (
                "*** Default values should not depend on " ^ 
                "controllable variables, \nwhich is the case of " ^
                (formula_to_string f) ^ ", the default value of " ^ 
                (Var.name v) ^ "\n");
             exit 2
            )
        | Some (Numer _) -> assert false
        | Some (Liste _l) -> assert false
        | None -> bddt, Some(toss_up_one_var (Var.name v))
      )
    | _  -> bddt, None

(** A map with
   - an accumulator 
   - a function returning an option, 
   - and where the None elements are removed from the resulting list 
*)
let (list_map_acc_option: ('acc -> 'a -> 'acc * 'b option) -> 'acc -> 'a list ->
     'acc * 'b list) =
  fun f x l ->
    let rec aux f x l =
      match l with
        [] -> x, []
      | e::t ->
        (match f x e with
            x, None -> (aux f x t)
          | x, Some(b) ->
            let x, res = aux f x t in
            x, b::res
        )
    in
    let x, res = aux f x l in
    x, res 


let rec (draw_in_bdd_rec: t -> Var.env_in -> Var.env -> int ->
         string -> Var.env * Store.t -> Bdd.t -> Bdd.t ->
         t * Var.env * Store.t' * Store.p) = 
  fun (snt, bddt) input memory vl ctx_msg (sl, store) bdd comb ->
    (* Returns [sl] appended to a draw of all the boolean variables
        bigger than the topvar of [bdd] according to the ordering
        induced by the comb [comb]. Also returns the (non empty) store
        obtained by adding to [store] all the numeric constraints that
        were encountered during this draw.

        Raises the [No_numeric_solution] exception whenever no valid
        path in [bdd] leads to a satisfiable set of numeric
        constraints.  
    *)
    if 
      Bdd.is_true bdd
    then
      (* Toss the remaining bool vars. *)
      let (store_int, store_poly) =
        try Store.switch_to_polyhedron_representation vl store
        with Store.No_polyedral_solution -> raise (No_numeric_solution (snt,bddt))
      in
      let vars_index = Bdd.list_of_support comb in
      let bddt, id = list_map_acc_option 
          (toss_up_one_var_index input memory vl ctx_msg)
          bddt
          vars_index
      in
      let sl = Value.OfIdent.add_list sl id in
      (* (List.append sl *)
      (*       Printf.eprintf "draw_in_bdd_rec: %s \n" (Value.OfIdent.to_string "" sl);  *)
      (*       Printf.eprintf "     vars_index: %s \n"  *)
      (*         (String.concat "," (List.map string_of_int vars_index));  *)
      (*       flush stderr;  *)
      (snt, bddt), sl, store_int, store_poly
    else (
      assert (not (Bdd.is_false bdd));
      assert (sol_number_exists bdd snt);
      let bddvar = Bdd.topvar bdd in
      let cstr = Formula_to_bdd.index_to_linear_constraint bddt bddvar in
      match cstr with
      | Bv(var) ->
        draw_in_bdd_rec_bool input memory vl ctx_msg var (sl, store) bdd comb (snt,bddt)
      | EqZ(e) ->
        draw_in_bdd_rec_eq input memory vl ctx_msg e (sl, store) bdd comb  (snt,bddt)
      | Ineq(ineq) ->
        draw_in_bdd_rec_ineq input memory vl ctx_msg ineq (sl, store) bdd comb (snt,bddt)
    )

and (draw_in_bdd_rec_bool: Var.env_in -> Var.env -> int -> string -> var -> 
     Var.env * Store.t -> Bdd.t -> Bdd.t -> t -> 
     t * Var.env * Store.t' * Store.p) = 
  fun input memory vl ctx_msg var (sl, store) bdd comb (snt, bddt) ->
    let bddvar = Bdd.topvar bdd in
    let topvar_comb  = Bdd.topvar comb in
    if bddvar <> topvar_comb then
      (* that condition means that topvar_comb is an unconstraint
         boolean var; hence we toss it up, or we compute its default
         value (cf "~default" flag). *)
      let bddt, new_sl = 
        match toss_up_one_var_index input memory vl ctx_msg bddt topvar_comb with
          bddt, Some(s) -> (*s::sl*) bddt, Value.OfIdent.add sl s
        | bddt, None -> bddt, sl
      in
      draw_in_bdd_rec (snt,bddt) input memory vl ctx_msg (new_sl, store) bdd
        (Bdd.dthen comb)
    else 
      (* bddvar = combvar *) 
      let (n, m) = sol_number (snt,bddt) bdd in
      let _ =
        if ((eq_sol_nb n zero_sol) && (eq_sol_nb m zero_sol))
        then (
          if vl > 3 then (print_string "BDD backtrack...\n"; flush stdout);
          raise (No_numeric_solution (snt, bddt))
        )
      in
      let (
        bdd1, bool1, sol_nb1,
        bdd2, bool2, sol_nb2
      ) =
        let ran = Random.float 1. 
        and sol_nb_ratio = float_of_sol_nb (div_sol_nb n  (add_sol_nb n m))
        in
        assert (not (Bdd.is_cst bdd));
        if 
          ran < sol_nb_ratio
          (* Depending on the result of a toss (based on the number of
             solutions in each branch), we try the [then] or the [else]
             branch first.  *)
        then
          ((Bdd.dthen bdd), true, n,
           (Bdd.delse bdd), false, m )
        else 
          ((Bdd.delse bdd), false, m,
           (Bdd.dthen bdd), true, n )
      in
      let (sl1, sl2, new_comb) =
        (* (((Var.name var), B(bool1))::sl),  *)
        (Value.OfIdent.add sl (Var.name var, B(bool1))), 
        (* (((Var.name var), B(bool2))::sl), *)
        (Value.OfIdent.add sl (Var.name var, B(bool2))), 
        (if Bdd.is_true comb then comb else Bdd.dthen comb) 
      in
      (* 
A solution will be found in this branch iff there exists
at least one path in the bdd that leads to a satisfiable
set of numeric constraints. If it is not the case,
[res_opt] is bound to [None]. 
*)
      try 
        if not (eq_sol_nb sol_nb1 zero_sol)
        then (
          assert (not (Bdd.is_false bdd1)) ;
          draw_in_bdd_rec (snt,bddt) input memory vl ctx_msg (sl1, store) bdd1 new_comb)
        else raise (No_numeric_solution (snt, bddt))
      with
        No_numeric_solution (snt, bddt) ->
        if not (eq_sol_nb sol_nb2 zero_sol)
        then (
          assert (not (Bdd.is_false bdd1)) ;
          draw_in_bdd_rec (snt,bddt) input memory vl ctx_msg (sl2, store) bdd2 new_comb)
        (* 
The second branch is now tried because no path in
the first bdd leaded to a satisfiable set of
numeric constraints. 
*) 
        else (       
          if vl > 3 then (print_string "BDD backtrack...\n"; flush stdout);
          raise (No_numeric_solution (snt,bddt))
        )
      | e ->
        print_string ((Printexc.to_string e) ^ "\n");
        flush stdout;
        assert false

and (draw_in_bdd_rec_ineq: Var.env_in -> Var.env -> int -> string -> Constraint.ineq -> 
     Var.env * Store.t -> Bdd.t -> 
     Bdd.t -> t -> t * Var.env * Store.t' * Store.p) = 
  fun input memory vl ctx_msg cstr (sl, store) bdd comb (snt,bddt) ->
    (* 
       When we add to the store an inequality constraint GZ(ne) or
       GeqZ(ne) ([split_store]), 2 stores are returned. The first is a
       store where the inequality has been added; the second is a
       store where its negation has been added.

       Whether we choose the first one or the second depends on a toss
       made according the (boolean) solution number in both branches
       of the bdd. If no solution is found into that branch, the other
       branch with the other store is tried.
    *)
    let (n, m) = sol_number (snt,bddt) bdd in
    let _ =
      if ((eq_sol_nb n zero_sol) && (eq_sol_nb m zero_sol))
      then (
        if vl > 3 then (
          print_string "BDD backtrack...\n";
          flush stdout
        );
        raise (No_numeric_solution (snt,bddt))
      )
    in 
    let ran = Random.float 1. in
    let cstr_neg = Constraint.neg_ineq cstr in
    let (cstr1, bdd1, _sol_nb1, cstr2, bdd2, sol_nb2) =
      if 
        ran < (float_of_sol_nb (div_sol_nb n (add_sol_nb n m)))
      then
        (cstr, (Bdd.dthen bdd), n, cstr_neg, (Bdd.delse bdd), m)
      else 
        (cstr_neg, (Bdd.delse bdd), m, cstr, (Bdd.dthen bdd), n)
    in
    let store1 = Store.add_constraint store bddt (Ineq cstr1) in
    (* A solution will be found in this branch iff there exists
       at least one path in the bdd that leads to a satisfiable
       set of numeric constraints. If it is not the case,
       [res_opt] is bound to [None]. *)

    try
      if not (Store.is_store_satisfiable vl store1) then 
        raise (No_numeric_solution (snt,bddt));
      draw_in_bdd_rec (snt,bddt) input memory vl ctx_msg (sl, store1) bdd1 comb
    with 
      No_numeric_solution (snt,bddt) -> 
      let store2 = Store.add_constraint store bddt (Ineq cstr2) in
      (* 
The second branch is tried if no path in the first
bdd leaded to a satisfiable set of numeric
constraints.  
*)
      if 
        (not (eq_sol_nb sol_nb2 zero_sol)) 
        && Store.is_store_satisfiable vl store2
      then
        draw_in_bdd_rec (snt,bddt) input memory vl ctx_msg (sl, store2) bdd2 comb
      else (
        if vl > 3 then (
          print_string "BDD backtrack...\n";
          flush stdout
        );
        raise (No_numeric_solution(snt,bddt))
      )
    | e ->
      print_string ((Printexc.to_string e) ^ "\n");
      flush stdout;
      assert false

and (draw_in_bdd_rec_eq: Var.env_in -> Var.env -> int -> string -> 
     Ne.t -> Var.env * Store.t ->
     Bdd.t -> Bdd.t -> t -> t * Var.env * Store.t' * Store.p) = 
  fun input memory vl ctx_msg ne (sl, store) bdd comb  (snt,bddt)  ->
    (*
      We consider 3 stores: 
      - store + [ne = 0]
      - store + [ne > 0]
      - store + [ne < 0]

      Whether we choose the first one or not depends on toss made
      according the (boolean) solution number in both branches of the
      bdd. If the else branch is choosen (if it is chosen in the
      first place, or if backtracking occurs because no solution is
      found in the then branch) whether we try the second or the
      third store first is (fairly) tossed up.
    *)
    let (n, m) = sol_number (snt,bddt) bdd in
    (*     let bddi = try hfindl "bddd: " bdd_to_int bdd  with _ -> -1 in  *)
    let _ =
      if ((eq_sol_nb n zero_sol) && (eq_sol_nb m zero_sol))
      then (
        if vl > 3 then (
          print_string "BDD backtrack...\n";
          flush stdout
        );
        raise (No_numeric_solution (snt,bddt)) 
      )
    in
    let ran0 = Random.float 1. 
    and ran  = Random.float 1. 
    and cstr = (EqZ ne)
    and not_cstr = (Ineq (GZ ne))
    and not_cstr2 = (Ineq (GZ (Ne.neg_nexpr ne)))
    in
    let sol_nb_ratio = float_of_sol_nb (div_sol_nb  n (add_sol_nb n m)) in
    let (
      cstr1, bdd1, sol_nb1, 
      cstr2, bdd2, sol_nb2,
      cstr3, bdd3, sol_nb3) =
      if 
        ran0 < 0.5
        (* 
When taking the negation of an equality, we can
either try > or <. Here, We toss which one we
will try first.  
*)
      then
        if 
          ran < sol_nb_ratio
  (*
Depending on the result of a toss (based on the number
of solution in each branch), we try the [then] or the
[else] branch first.  
*)
        then
          (cstr,      (Bdd.dthen bdd), n,
           not_cstr,  (Bdd.delse bdd), m,
           not_cstr2, (Bdd.delse bdd), m)
        else 
          (not_cstr,  (Bdd.delse bdd), m,
           cstr,      (Bdd.dthen bdd), n,
           not_cstr2, (Bdd.delse bdd), m)
      else
      if
        ran < sol_nb_ratio
        (* Ditto *)
      then
        (cstr,      (Bdd.dthen bdd), n,
         not_cstr2, (Bdd.delse bdd), m,
         not_cstr,  (Bdd.delse bdd), m)
      else 
        (not_cstr2, (Bdd.delse bdd), m,
         cstr,      (Bdd.dthen bdd), n,
         not_cstr,  (Bdd.delse bdd), m)
    in    
    let store1 = Store.add_constraint store bddt cstr1 in 
    (* 
A solution will be found in this branch iff there exists
at least one path in the bdd that leads to a satisfiable
set of numeric constraints. 
      *)
    try
      (
        if 
          (not (Store.is_store_satisfiable vl store1)) || (sol_nb1 = zero_sol)
        then 
          raise (No_numeric_solution (snt,bddt))
      );
      draw_in_bdd_rec (snt,bddt) input memory vl ctx_msg (sl, store1) bdd1 comb
    with 
      No_numeric_solution (snt,bddt) -> 
      (* 
The second branch is tried if no path in the first bdd
leaded to a satisfiable set of numeric constraints. 
*)
      let store2 = Store.add_constraint store bddt cstr2 in
      try
        if 
          (not (Store.is_store_satisfiable vl store2)) || (sol_nb2 = zero_sol) 
        then 
          raise (No_numeric_solution (snt,bddt));
        draw_in_bdd_rec (snt,bddt) input memory vl ctx_msg (sl, store2) bdd2 comb
      with 
        No_numeric_solution (snt,bddt) -> 
        let store3 = Store.add_constraint store bddt cstr3 in
        if 
          (not (Store.is_store_satisfiable vl store3)) || (sol_nb3 = zero_sol) 
        then (
          if vl > 3 then (
            print_string "BDD backtrack...\n";
            flush stdout
          );
          raise (No_numeric_solution (snt,bddt))
        );
        draw_in_bdd_rec (snt,bddt) input memory vl ctx_msg (sl, store3) bdd3 comb
      | e -> 
        print_string ((Printexc.to_string e) ^ "\n");
        flush stdout;
        assert false


(* exported *)
let (draw_in_bdd: t -> 
           Var.env_in -> Var.env -> int -> string -> var list -> Bdd.t ->
          Bdd.t -> t * Var.env * Store.t' * Store.p) = 
  fun t input memory vl ctx_msg num_vars_to_gen bdd comb ->
    let t = build_sol_nb_table t num_vars_to_gen bdd comb in
    draw_in_bdd_rec t input memory vl ctx_msg
      (Value.OfIdent.empty, Store.create num_vars_to_gen) bdd comb




(****************************************************************************)

(*
type sol_nb = float
let add_sol_nb =  (+.)   
let mult_sol_nb n m = n *. m   
let zero_sol = 0.   
let one_sol = 1.   
let eq_sol_nb = (=)   

let two_power_of n = 
  let res =  2. ** (float_of_int n) in
  let _ = assert (res <> infinity) in
    res

let float_of_sol_nb sol = sol   
let string_of_sol_nb = string_of_float   
*)

