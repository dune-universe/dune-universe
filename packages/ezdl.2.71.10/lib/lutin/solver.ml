(* -----------------------------------------------------------------------
** Copyright (C) 2001-2003 - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: solver.ml
** Main author: erwan.jahier@univ-grenoble-alpes.fr
*)

open List
open Exp
open Util
open Value
open Var

(****************************************************************************)

let sol_number = Bddd.sol_number
let draw_in_bdd =  Bddd.draw_in_bdd
let add_snt_entry = Bddd.add_snt_entry
let init =  Bddd.init_snt

type t = Bddd.t
let tbl_to_string = Bddd.tbl_to_string
(****************************************************************************)
(****************************************************************************)

(* exported *)
let (is_satisfiable_bdd: t -> 
       Var.env_in -> Var.env -> int -> string -> formula -> string -> t * bool * Bdd.t) =
  fun tbl input memory vl ctx_msg f msg -> 
    let tbl, bdd = Bddd.formula_to_bdd tbl input memory ctx_msg vl f in
    let is_sat1 = not (Bdd.is_false bdd) 
    and is_sat2 =
      (* the formula may be false because of numerics. in that case,
         the solution number is set to (0,0) (if a draw has been tried).
      *)
      (
        try
          let (n, m) = sol_number tbl bdd in
            not ((Sol_nb.zero_sol, Sol_nb.zero_sol) = (n, m))
        with
            Not_found -> true
              (* migth be false because of numerics, but we don't know yet *)
          | e ->
              print_string ((Printexc.to_string e) ^ "\n");
              flush stdout;
              assert false
      )
    in
      if vl > 2 then
        (
          print_string ("\n[" ^ msg^ "] -> ");
          print_string (formula_to_string f);
          if is_sat1 && is_sat2
          then print_string "\n   is satisfiable (for the Boolean part).\n"
          else if is_sat1 
          then print_string "\n   is not satisfiable (because of numerics).\n"
          else print_string "\n   is not satisfiable (because of Booleans).\n";
          flush stdout
        );
      tbl, is_sat1 && is_sat2, bdd

(* Exported *)
let (is_satisfiable: t -> Var.env_in -> Var.env -> int -> string -> formula
     -> string -> t * bool) =
  fun tbl input memory vl ctx_msg f msg ->
    let tbl, res, _ = is_satisfiable_bdd tbl input memory vl ctx_msg f msg in
    tbl, res

(****************************************************************************)

(****************************************************************************)
(* printing bbd's with dot *)

open Sol_nb

let cpt = ref 0 
(* let (bdd_to_int : (Bdd.t, int) Hashtbl.t)= Hashtbl.create 100  *)

let (bdd_to_int : (Bdd.t, int) Hashtbl.t)= Hashtbl.create 100 

(** for printing purposes only  *)
let (bdd_to_graph: t -> Bdd.t -> (int -> string) -> bool -> 
       (string * string * string) list) =
  fun snt bdd index_to_string only_true ->
    let _ =
      cpt := 0;
      Hashtbl.clear bdd_to_int 
    in
    let get_label bdd =
      if Bdd.is_true bdd then "True"
      else if Bdd.is_false bdd then "False"
      else 
   let bdd_int = 
     try
       hfind bdd_to_int  bdd
     with Not_found -> 
       incr cpt;
       Hashtbl.add  bdd_to_int  bdd !cpt;
       !cpt
   in
     ("\"(" ^(string_of_int bdd_int) ^ ")" ^ (index_to_string (Bdd.topvar bdd))) 
    in
    let rec (bdd_to_graph_acc: (string * string * string) list -> Bdd.t list -> 
          bool -> Bdd.t -> (string * string * string) list * Bdd.t list) =
      fun acc0 visited only_true bdd ->
   if Bdd.is_cst bdd then (acc0, visited)
   else
     if List.mem bdd visited then (acc0, visited) else
     let bddt = Bdd.dthen bdd in
     let bdde = Bdd.delse bdd in
     let label = get_label  bdd in
     let labelt = get_label bddt in
     let labele = get_label bdde in
     let s1,s2 = try sol_number snt bdd with _  -> zero_sol,zero_sol in
     let s1_str = " (" ^ Sol_nb.string_of_sol_nb s1 ^ ")"
     and s2_str = " (" ^ Sol_nb.string_of_sol_nb s2 ^ ")" 
     in
     let acc1 = 
       if s1 = zero_sol && only_true then acc0 else 
         ((label,"\""^ "then" ^ s1_str ^ "\"", labelt)::acc0) 
     in
     let acc2 = 
       if s2 = zero_sol && only_true then acc1 else 
         ((label, "\""^ "else" ^ s2_str ^ "\"", labele)::acc1) 
     in
     let (acc3, visited') = 
       bdd_to_graph_acc acc2 (bdd::visited) only_true bddt 
     in
       bdd_to_graph_acc acc3 visited' only_true bdde
    in
      fst (bdd_to_graph_acc [] [] only_true bdd)


let index_to_vn t i = (
  try
    Constraint.to_string (Bddd.index_to_linear_constraint t i)^"\""
    with _ -> "\""
)
  
let (print_bdd_with_dot: t -> Bdd.t -> string -> bool -> unit) =
  fun snt bdd label only_true ->
    let arcs = bdd_to_graph snt bdd (index_to_vn snt) only_true in
    let dot_file = label ^ ".dot" in
    let pdf = label ^ ".pdf" in
    let dot_oc = open_out dot_file in
    let put = output_string dot_oc in
    let _ =
      put "digraph G {\n\n ordering=out;\n\n";
      put " ratio = compress;\n\n";
      List.iter (fun (n, _, _) -> put ("\t" ^ n ^ "\t ; \n")) arcs ;
      List.iter (fun (f, l, t) -> put ("\t" ^ f ^ " -> " ^ t ^ "\t  [label = " ^ l ^ " ] ; \n")) arcs ;
      put "} \n";
      flush dot_oc ;
      close_out dot_oc
    in
    (*  Calling dot to create the postscript file *)
    let cmd = ("dot -Tpdf " ^ dot_file ^ " -o " ^ pdf) in
    let exit_code_dot = Sys.command cmd in
    if (exit_code_dot <> 0)
    then Printf.printf "The sys call '%s' failed (exit code %i)\n\n" cmd exit_code_dot
    else (Printf.printf "%s generated (with '%s')" pdf cmd)


(****************************************************************************)
(****************************************************************************)

let (compute_default_value : t -> Var.env_in -> Var.env -> int -> string -> Store.t' ->
     Exp.var list -> t * Store.t' * num_subst list) =
  fun t input memory vl ctx_msg st varl ->
    (* [varl] is the list of variable that have not been constrained at all.
       For the variables that have a default value, we compute them and remove
       them from the variables that still need to be drawned.
    *)
    let (maybe_compute_value : t * Store.t' * num_subst list -> Exp.var ->
         t * Store.t' * num_subst list) =
      fun acc var ->
        match (Var.default var) with
          None -> acc
        | Some (Numer e) ->
          let t,st,l = acc in
          let t, gne = Bddd.num_to_gne t input memory ctx_msg vl e in
          (match Gne.get_constant gne with
             None ->
             print_string (
               "*** Default values should not depend on " ^
               "controllable variables, \nwhich is the case of " ^
               (Exp.num_to_string e) ^ ", the default value of " ^
               (Var.name var) ^ "\n");
             exit 2
           | Some (I i) ->
             t, Store.remove_var_from_range_store st var, (((Var.name var), (I i))::l)
           | Some (F f) ->
             t, Store.remove_var_from_range_store st var, (((Var.name var), (F f))::l)
          )
        | Some _ -> assert false
    in
    List.fold_left maybe_compute_value (t, st, []) varl


let (draw : Var.env_in -> Var.env -> int -> string -> Var.name list list ->
     Thickness.numeric -> Var.name list -> var list -> Bdd.t -> t -> Bdd.t ->
     t * (Var.env * Var.env) list) =
  fun input memory vl msg output_var_names (k1, k2, vertices_nb)
    _bool_vars_to_gen num_vnt_to_gen comb snt bdd ->
    (* Draw the output and local vars to be generated by the environnent. *)

    let (snt, bool_subst_l, store_range0, store_poly) =
      draw_in_bdd snt input memory vl msg num_vnt_to_gen bdd comb
    in
    let snt, num_subst_ll0 =
      if num_vnt_to_gen = [] then snt, []
      else
        let (snt, store_range, sl0) =
          compute_default_value snt input memory vl msg store_range0
            (Store.get_untouched_var store_range0)
        in
        (* XXX I should do something there if Draw.inside raises 
           No_numeric_solution, to be able to backtrack in the bdd,
           update the snt, and so on. 

           Not so easy...

           For that reason, instead of raising that excp when I cannot
           find any valid integers, i return a wrong one...
           *)
        let sll1 = Draw.inside vl store_range store_poly k1 sl0 in
        let sll2 = Draw.edges vl store_range store_poly k2 sl0 in
        let sll3 =
          match vertices_nb with
          | Thickness.All ->
            let vl = Draw.get_all_vertices store_range store_poly sl0 in
            assert (vl <> []);
            vl
          | Thickness.AtMost k3 ->
            Draw.vertices vl store_range store_poly k3 sl0
        in
        snt, rev_append sll1 (rev_append sll2 sll3)
    in
    let numlist2map nl = List.fold_left
        (fun acc (vn, num) -> Value.OfIdent.add acc (vn, N num)) Value.OfIdent.empty nl in
    let num_subst_ll =
      (* List.map (List.map (fun (vn, num) -> (vn, N num))) num_subst_ll0 *)
      List.map numlist2map num_subst_ll0
    in
    let subst_ll = if num_subst_ll = []
      then
        [bool_subst_l]
      else
        List.map
          (fun num_subst_l -> Value.OfIdent.union bool_subst_l num_subst_l)
          num_subst_ll
    in
      (*
        assert (
(List.for_all
(fun (subst_l:Var.env) ->
(*  Checks that we have generated all variables, and not more. *)
(* let (gen_vars, _) = List.split subst_l in *)
let gen_vars = Value.OfIdent.support subst_l in
let num_vars_to_gen =
List.map (fun var -> (Var.name var)) num_vnt_to_gen
in
let vars_to_gen = append bool_vars_to_gen num_vars_to_gen in
let gen_vars_sorted = (sort (compare) gen_vars) in
let vars_to_gen_sorted = (sort (compare) vars_to_gen) in
if gen_vars_sorted = vars_to_gen_sorted then true else
(
output_string stderr " \ngen vars:   \t";
List.iter
(fun vn -> output_string stderr (vn ^ " ")) gen_vars_sorted;
output_string stderr " \nvars to gen:\t";
List.iter
(fun vn -> output_string stderr (vn ^ " ")) vars_to_gen_sorted;
output_string stderr " \n";
(try
List.iter2 
(fun vn1 vn2 -> 
if vn1 <> vn2 then 
(
output_string stderr ("\n e.g., " ^ vn1 ^ " <> " ^ vn2 ^ "\n");
failwith ""
)
)
gen_vars_sorted 
vars_to_gen_sorted
with _ -> 
();
);
print_bdd_with_dot bdd   
(fun i ->   
Constraint.to_string   
(Formula_to_bdd.index_to_linear_constraint i)^"\""  
)  
"debug_bdd"
true;     
print_bdd_with_dot bdd   
(fun i ->   
Constraint.to_string   
(Formula_to_bdd.index_to_linear_constraint i)^"\""  
)  
"debug_bdd_all"
false;
false
)
)
subst_ll
)
        );
      *)
    (* Splits output and local vars. *)
    let out_vars = List.flatten output_var_names in
    snt,
    List.map
      (fun subst_l ->
         (Value.OfIdent.partition
            (fun (vn, _) ->
               List.exists (fun out_var -> out_var = vn) out_vars)
            subst_l
         )
      )
      subst_ll

(****************************************************************************)

let rec (unfold:  ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a -> int -> 'acc * 'b list) =
  fun f acc i n ->
    assert (n >= 0);
    unfold_do f acc i n

and unfold_do f acc i =
  function
  | 0 -> acc, []
  | n ->
    let acc, x = f acc i in
    let acc, xl = unfold f acc i (n-1) in
    acc, x::xl
         

(* Exported *)
let (solve_formula: t -> Var.env_in -> Var.env -> int -> string -> Var.name list list ->
     Thickness.formula_draw_nb ->  Thickness.numeric -> formula -> var list ->
     formula -> t * (Var.env * Var.env) list) =
  fun snt input memory vl ctx_msg output_var_names p num_thickness bool_vars_to_gen_f
    num_vars_to_gen f ->

    let snt, bdd = (Bddd.formula_to_bdd snt input memory ctx_msg vl f) in
    let _ = 
      assert (not (Bdd.is_false bdd));
      if vl >= 2 then
        (
          print_string "\n --> ";
          print_string (formula_to_string f);
          print_string
            "\n  has been elected to be the formula in which solutions are drawn for ";
          print_string (formula_to_string bool_vars_to_gen_f);
          print_string "\n";
          flush stdout
        )
    in

    let snt, comb0 =
      (Bddd.formula_to_bdd snt input memory ctx_msg vl bool_vars_to_gen_f)
    in
    let comb =
      (* All Boolean vars should appear in the comb so that when we
         find that such a var is missing along a bdd path, we can
         perform a (fair) toss for it. On the contrary, if a
         numerical contraint disappear from a bdd (eg, consider [(f
         && false) || true]), it is not important; fairly tossing a
         (boolean) value for a num constaint [nc] and performing a
         fair toss in the resulting domain is equivalent to directly
         perform the toss in the (unconstraint wrt [nc]) initial
         domain.
      *)
      Bdd.dand (Bdd.support bdd) comb0
    in   
    let bool_vars_to_gen = Exp.support bool_vars_to_gen_f in
    try
      let snt, res =
        (unfold
           (draw input memory vl ctx_msg output_var_names num_thickness
              bool_vars_to_gen num_vars_to_gen comb)
           snt
           bdd
           p
        )
      in
      let res = flatten res in
      snt, res
    with
      Bddd.No_numeric_solution snt ->
      if vl >= 3 then (
        (print_bdd_with_dot snt bdd "debug_bdd" true);     
        (print_bdd_with_dot snt bdd "debug_bdd_all" false);     
      );
      let snt = add_snt_entry snt bdd (Sol_nb.zero_sol, Sol_nb.zero_sol) in
      if vl >= 2 then  
        (
          print_string ("[draw] -> " ^ (formula_to_string f) ^ 
                        "\n    is not satisfiable because of numerics.\n");
          (* Bdd.print_mons bdd; *)
        );
      snt, []

    | e ->
      print_string ((Printexc.to_string e) ^ "\n");
      flush stdout;
      assert false

(* exported *)
let (eval_int_expr:  t -> Exp.num -> string -> Var.env_in -> Var.env -> int
     -> int option) =
  fun snt e msg input mem vl ->
    Bddd.eval_int_expr snt e msg input mem vl

