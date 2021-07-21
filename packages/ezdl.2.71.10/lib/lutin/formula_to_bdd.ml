(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: formula_to_bdd.ml
** Main author: erwan.jahier@univ-grenoble-alpes.fr
*)

open Value
open Exp
open Constraint


let output_msg msg =
  output_string stderr msg;
  flush stderr

(** [lookup input pre vn] looks up the value of vn in [pre] and [input] *)
let rec (lookup: Var.env_in -> Var.env -> string -> int -> Exp.var -> Value.t option) =
  fun input memory ctx_msg vl var ->
    match (Var.mode var) with
    | Var.Output -> None
    | Var.Local -> None
    | Var.Input -> (
        try Some(Var.get_val_env_in input (Var.name var))
        with _ ->
          output_msg ctx_msg;
          exit 2
      )
    | Var.Pre -> (
        try
          (* Some(List.assoc (Var.name var) memory) *)
          Some(Value.OfIdent.get memory (Var.name var))
        with
        (* if not in memory, look at a possible init value *)
          Not_found ->
          match (Var.init var) with
          | Some (Numer (Ival i)) -> Some (N (I i))
          | Some (Numer (Fval f)) -> Some (N (F f))
          | Some (Numer (Ivar vi)) -> lookup input memory ctx_msg vl vi
          | Some (Numer (Fvar vf)) -> lookup input memory ctx_msg vl vf
          | Some (Formu  True) -> Some (B true)
          | Some (Formu  False) -> Some (B false)
          | Some (Liste _) -> 
            assert false (* XXX not sure ...*)
          (*           | Some (Liste [Nu (Ival i)]) ->  *)
          (*          Some (N (I i)) *)

          | Some _ ->
            (* Structured expression ougth to have been
               flattened before *)
            output_msg (
              "*** Error: the initial value of variable " 
              ^ (Prevar.get_root_var_name (Var.name var)) ^
              "  is not valid.\n");
            exit 2
          | None ->
            output_msg (
              "*** The expression " ^ (Prevar.format (Var.name var))
              ^ " is undefined at current step " ^ ctx_msg
              ^ "*** You need to rewrite your environment"
              ^ " spec in such a way that no pre is used"
              ^ " for that variable at that step\n    "
              ^ " or to give it an initial value.\n"^ ctx_msg);
            exit 2
      )


(****************************************************************************)
(****************************************************************************)

(* Transforming a formula into a bdd is expensive; this is why we
   store the result of this transformation in (internal) tables.

   Moreover, formula that does not depend on pre nor input vars are
   stored in a different table that is not cleared at each new step
   (cf [bdd_global] and [set_bdd_global] versus [bdd] and [set_bdd]).
    Ditto for the linear_constraint <-> bdd index tables.

   The reason for that is that, formula that depends on pre or input
   are not likely to appear twice if they contains numeric values.
   For Boolean, well, I could leave them in the global table.
*)

module IntMap = struct
  include Map.Make(struct type t = int let compare = compare end)
end
module CstrMap = struct
  include Map.Make(struct type t = Constraint.t let compare = compare end)
end
module FormulaMap = struct
  include Map.Make(struct type t = Exp.formula let compare = compare end)
end

type t = {
  index_cpt : int; (* This counter is used to provide fresh var indexes *)
  free_index_list: int list; 
  (* List of currently unused bdd var indexes. When we need an index,
     we first look in this list before creating a new index. This is to avoid
     the creation of useless bdd index, which considerably slow down the
     Bdd package.
  *)
  bdd_manager: unit; (* for some othe bdd packages, it can be something else *)
  lc2i_tbl : int CstrMap.t;
  i2lc_tbl : Constraint.t IntMap.t;
  global_lc2i_tbl : int CstrMap.t;
  global_i2lc_tbl : Constraint.t IntMap.t;
  bdd_tbl        : Bdd.t FormulaMap.t ;
  bdd_tbl_global : Bdd.t FormulaMap.t ;
}

let init_manager () = Bdd.init ~pagesize:50000 ~verbose:true ()
let init () =
  {
    index_cpt = 0; 
    free_index_list =  [];
    bdd_manager = init_manager () ;
    lc2i_tbl = CstrMap.empty;
    i2lc_tbl = IntMap.empty;
    global_lc2i_tbl = CstrMap.empty;
    global_i2lc_tbl = IntMap.empty;
    bdd_tbl =  FormulaMap.empty ;
    bdd_tbl_global = FormulaMap.empty ;
  }

let tbl_to_string t =
  Printf.sprintf " {
    index_cpt=%i;
    free_index_list = %s;
    bdd_manager = init_manager () ;
    |lc2i_tbl|= %i;
    |i2lc_tbl| = %i;
    |global_lc2i_tbl| = %i;
    |global_i2lc_tbl| = %i;
    |bdd_tbl| =  %i ;
    |bdd_tbl_global| = %i;
}"
    t.index_cpt
    (String.concat "," (List.map string_of_int t.free_index_list))
    (CstrMap.cardinal t.lc2i_tbl)
    (IntMap.cardinal t.i2lc_tbl)
    (CstrMap.cardinal t.global_lc2i_tbl)
    (IntMap.cardinal t.global_i2lc_tbl)
    (FormulaMap.cardinal t.bdd_tbl)
    (FormulaMap.cardinal t.bdd_tbl_global)
    
  
(* returns a free index and updates [free_index_list] *)
let (get_an_index : t -> t * int) =
  fun t ->
      match t.free_index_list with
      | [] ->  { t with index_cpt = t.index_cpt+1 }, t.index_cpt+1
      | i::tail -> { t with free_index_list = tail }, i
   
let (free_indexes : t -> int list -> t) =
  fun t il ->
    (* We sort the list because the initial bdd order is generally the best *)
    { t with
      free_index_list = List.sort (-) (List.rev_append il t.free_index_list)
    }


(* Returns the bdd of a formula if it is available in the cache,
  raises [Not_found] otherwise. *)
let (bdd : t -> Exp.formula  -> Bdd.t) =
  fun t f -> FormulaMap.find f t.bdd_tbl 


let (bdd_global : t -> Exp.formula  -> Bdd.t) =
  fun t f -> FormulaMap.find f t.bdd_tbl_global 

(** Stores the correspondance between a formula and a bdd. *)
let (set_bdd : t -> Exp.formula  -> Bdd.t -> t) =
  fun t f bdd ->
    { t with bdd_tbl = FormulaMap.add f bdd t.bdd_tbl }

let (set_bdd_global : t -> Exp.formula  -> Bdd.t -> t) =
  fun t f bdd -> 
    { t with bdd_tbl_global = FormulaMap.add f bdd t.bdd_tbl_global }


(* exported *)
let (get_index_from_linear_constraint : t -> Constraint.t -> int) =
  fun t f -> 
    try CstrMap.find f t.lc2i_tbl 
    with Not_found ->
    try  CstrMap.find f t.global_lc2i_tbl
    with Not_found ->
      -1

let (linear_constraint_to_index : t -> Constraint.t -> bool -> t * int) =
  fun t f depend_on_input -> 
    if depend_on_input then
      (
        try t, CstrMap.find f t.lc2i_tbl
        with Not_found ->
          let t, index = get_an_index t in
          { t with
            lc2i_tbl = CstrMap.add  f index t.lc2i_tbl;
            i2lc_tbl = IntMap.add index f t.i2lc_tbl;
            index_cpt = index;
          }, index
      )
    else
      (
        try t, CstrMap.find f t.global_lc2i_tbl 
        with Not_found ->
          let t, index = get_an_index t in
          { t with
            global_lc2i_tbl = CstrMap.add  f index t.global_lc2i_tbl;
            global_i2lc_tbl = IntMap.add index f t.global_i2lc_tbl;
            index_cpt = index;
          }, index
      )

(* exported *)
let (index_to_linear_constraint : t -> int -> Constraint.t) =
  fun t i ->
    try IntMap.find i t.global_i2lc_tbl
    with Not_found ->
    try IntMap.find i t.i2lc_tbl with _ ->
      failwith (
        Printf.sprintf "Error: can not find index %i in Formula_to_bdd tables\n" i)


(****************************************************************************)
(* Clearing table procedures *)

let (clear_linear_constraint_index : t -> t) =
  fun t ->
    let index_to_free = IntMap.fold (fun index _ acc -> index::acc) t.i2lc_tbl [] in
    let t = free_indexes t index_to_free in
    { t with
      lc2i_tbl = CstrMap.empty;
      i2lc_tbl = IntMap.empty;
    }

let (clear_global_linear_constraint_index : t -> t) =
  fun t ->
    { t with
      index_cpt = 0;
      free_index_list = [];    
      lc2i_tbl  = CstrMap.empty;
      i2lc_tbl = IntMap.empty;
      global_lc2i_tbl = CstrMap.empty;
      global_i2lc_tbl = IntMap.empty;
    }



(* Exported *)
let  (clear_step : t -> t) =
  fun t ->
    let t = clear_linear_constraint_index t in
    { t with
    bdd_tbl = FormulaMap.empty ;
    bdd_tbl_global = FormulaMap.empty ;
    }


(* Exported *)
let (clear_all : t -> t) =
  fun t ->
    let t = clear_global_linear_constraint_index t in
    { t with
    bdd_tbl = FormulaMap.empty ;
    bdd_tbl_global = FormulaMap.empty ;
    bdd_manager = init_manager () 
    }

(****************************************************************************)
(****************************************************************************)

type comp = SupZero | SupEqZero | EqZero

(* 
  The bool associated the bdd says whether or not the result 
   of the translation should be tabulated or not. 

   The heuristic is to avoid the tabulation of expressions that
   depends on 
   - float
   - memory
   - input 

   true means : do not store

   it's just an heuristic anyway
*)

let translate_nor fl =
  assert (fl <> []);
  let aux facc f = And(facc, Not(f)) in
  List.fold_left aux (Not (List.hd fl)) (List.tl fl)
  
let translate_nary_and fl = (* n-ary "and" *)
  assert (fl <> []);
  let aux facc f = And(facc, f) in
  List.fold_left aux (List.hd fl) (List.tl fl)
  
let translate_nary_or fl = (* n-ary "or" *)
  assert (fl <> []);
  let aux facc f = Or(facc, f) in
  List.fold_left aux (List.hd fl) (List.tl fl)
  
let translate_nxor fl =
  let n = List.length fl in
  let split i l =
    let rec aux i acc = function
    | [] -> assert false
    | x::tail -> if i = 0 then x, List.rev_append acc tail else aux (i-1) (x::acc) tail
    in
    aux i [] l
  in
  let rec aux acc i =
    if i = n then acc else
      let cur =
        let x,l = split i fl in
        And(x, Nor(l))
      in
      let acc = Or(cur, acc) in
      aux acc (i+1)
  in
  aux False 0
    
let _translate_nxor_alt fl =
  (* Using the following formula:
     XOR(ϕ1,ϕ2,...,ϕn) = OR(ϕ1,ϕ2,...,ϕn) ∧ AND ¬(ϕi∧ϕj)
                                             i<j≤n             

 It uses twice less connectors than the previous one, but it happens to be 10x slower!
*)
  assert (fl <> []);
  let or_part = translate_nary_or fl in
  let gen_pairs l =
    let rec aux acc l =
      match l with
      | [] -> acc
      | x::tail ->
        let cur_pairs = List.map (fun y -> x,y) tail in
        let acc = List.rev_append cur_pairs acc in
        aux acc tail 
    in
    aux [] l
  in
  let pairs: (Exp.formula * Exp.formula) list = gen_pairs fl in
  let pairs: Exp.formula list = List.map (fun (f1, f2) -> Not(And(f1,f2))) pairs in
  let and_part = translate_nary_and pairs in
  And(or_part, and_part)

let translate_diese fl = Or(translate_nor fl, translate_nxor fl)


let rec (translate_do : t -> Var.env_in -> Var.env -> string -> int -> Exp.formula ->
         t * Bdd.t * bool) =
  fun t input memory ctx_msg vl f ->
    try (t, bdd t f, true)
    with Not_found ->
    try (t, bdd_global t f, false)
    with Not_found ->
      let (t, bdd, dep) =
        match f with
          Not(f1) ->
          let (t, bdd_not, dep) =  (translate_do t input memory ctx_msg vl f1) in
          (t, Bdd.dnot bdd_not, dep)

        | Or(f1, f2) ->
          let (t, bdd1, dep1) = (translate_do t input memory ctx_msg vl f1) in
          let (t, bdd2, dep2) = (translate_do t input memory ctx_msg vl f2) in
          (t, (Bdd.dor bdd1 bdd2), dep1 || dep2)

        | And(f1, f2) ->
          let (t, bdd1, dep1) = (translate_do t input memory ctx_msg vl f1) in
          let (t, bdd2, dep2) = (translate_do t input memory ctx_msg vl f2) in
          (t, Bdd.dand bdd1 bdd2, dep1 || dep2)

        | Impl(f1, f2) ->
          let (t, bdd1, dep1) = (translate_do t input memory ctx_msg vl f1) in
          let (t, bdd2, dep2) = (translate_do t input memory ctx_msg vl f2) in
          (t, Bdd.dor (Bdd.dnot bdd1) bdd2, dep1 || dep2)

        | Xor(f1, f2) ->
          let (t, bdd1, dep1) = (translate_do t input memory ctx_msg vl f1) in
          let (t, bdd2, dep2) = (translate_do t input memory ctx_msg vl f2) in 
          (t, Bdd.xor bdd1 bdd2, dep1 || dep2)

        | NXor(fl) -> translate_do t input memory ctx_msg vl (translate_nxor fl)
        | Diese(fl) -> translate_do t input memory ctx_msg vl (translate_diese fl)
        | Nor(fl) -> translate_do t input memory ctx_msg vl (translate_nor fl)
        | IteB(f1, f2, f3) ->
          let (t, bdd1, dep1) = (translate_do t input memory ctx_msg vl f1) in
          let (t, bdd2, dep2) = (translate_do t input memory ctx_msg vl f2) in
          let (t, bdd3, dep3) = (translate_do t input memory ctx_msg vl f3) in
          (t, (Bdd.ite bdd1 bdd2 bdd3), dep1 || dep2 || dep3 )

        | True ->  (t, Bdd.dtrue  t.bdd_manager, false)
        | False -> (t, Bdd.dfalse t.bdd_manager, false)
        | Bvar(v) ->
          ( match (lookup input memory ctx_msg vl v) with
              Some(B(bool)) ->
              if bool
              then (t, Bdd.dtrue  t.bdd_manager, true)
              else (t, Bdd.dfalse t.bdd_manager, true)
            | Some(x) ->
              output_msg (
                "\n*** Type error: " ^  (Value.to_string x) ^
                "(" ^ (formula_to_string f) ^ ") ougth to be a Boolean.\n");
              exit 2
            | None ->
              (match (Var.alias v) with
                 Some (Formu(fa)) -> translate_do t input memory ctx_msg vl fa
               | Some (Numer(_e)) -> assert false
               | Some (Liste _l)  -> assert false
               | None ->
                 assert ((Var.mode v) <> Var.Input);
                 let t, i = linear_constraint_to_index t (Bv(v)) false in
                 (t, Bdd.ithvar i, false)
              )
          )

        | EqB(f1, f2) ->
          let (t, bdd1, dep1) = (translate_do t input memory ctx_msg vl f1) in
          let (t, bdd2, dep2) = (translate_do t input memory ctx_msg vl f2) in
          (t, Bdd.eq bdd1 bdd2, dep1 || dep2)

        | Eq(e1, e2) ->
          let t, gne = expr_to_gne t (Diff(e1, e2)) input memory ctx_msg vl in
          let t, bdd, dep = (gne_to_bdd t gne EqZero) in
          t, bdd, dep

        | SupEq(e1, e2) ->
          let t,gne = expr_to_gne t (Diff(e1, e2)) input memory ctx_msg vl in
          let t, bdd, dep = (gne_to_bdd t gne SupEqZero)in
          t, bdd, dep

        | Sup(e1, e2)   ->
          let t, gne = expr_to_gne t (Diff(e1, e2)) input memory ctx_msg vl in
          let t, bdd, dep = (gne_to_bdd t gne SupZero) in
          t, bdd, dep

        | InfEq(e1, e2) ->
          let t, gne = expr_to_gne t (Diff(e2, e1)) input memory ctx_msg vl in
          let t, bdd, dep = (gne_to_bdd t gne SupEqZero) in
          t, bdd, dep

        | Inf(e1, e2)   ->
          let t, gne =  expr_to_gne t (Diff(e2, e1)) input memory ctx_msg vl in
          let t, bdd, dep = (gne_to_bdd t gne SupZero) in
          t, bdd, dep
      in
      let t = if dep then (
          let t = set_bdd t f bdd in
          let t = 
            match f with
            | Not(_nf) -> t (* Already in the tbl thanks to the rec call *)
            | _  -> set_bdd t (Not(f)) (Bdd.dnot bdd)
          in
          t
        )
        else
          (* [f] translate_does not depend on pre nor input memory ctx_msg vl vars *)
          ( let t = set_bdd_global t f bdd in
            let t = 
            match f with
              Not(_nf) -> t (* Already in the table thanks to the rec call *)
            | _  -> set_bdd_global t (Not(f)) (Bdd.dnot bdd)
            in
            t
          )
      in      
      (t, bdd, dep)
and
  (num_to_gne: t -> Var.env_in -> Var.env -> string -> int -> Exp.num -> t * Gne.t) =
  fun t input memory ctx_msg vl e ->
    expr_to_gne t e input memory ctx_msg vl
and
  (expr_to_gne: t -> Exp.num -> Var.env_in -> Var.env -> string -> int -> t * Gne.t) =
  fun t e input memory ctx_msg vl ->
    (* Evaluates pre and input vars appearing in [e] and tranlates
        it into a so-called garded normal form.  *)
    match e with
      Sum(e1, e2) ->
      let t, gne1 = (expr_to_gne t e1 input memory ctx_msg vl) in
      let t, gne2 = (expr_to_gne t e2 input memory ctx_msg vl) in
      t, Gne.add gne1 gne2
    | Uminus e ->
      let t, gne = (expr_to_gne t e input memory ctx_msg vl) in
      t, Gne.opposite gne

    | Inf_int ->
      assert false (* should not occur since only weigth can be infinite
                      and this checked at parsing ... *)

    | Diff(e1, e2) ->
      let t, gne1 = (expr_to_gne t e1 input memory ctx_msg vl) in
      let t, gne2 = (expr_to_gne t e2 input memory ctx_msg vl) in
      t, Gne.diff gne1 gne2

    | Prod(e1, e2) ->
      let t, gne1 = (expr_to_gne t e1 input memory ctx_msg vl) in
      let t, gne2 = (expr_to_gne t e2 input memory ctx_msg vl) in
      t, Gne.mult gne1 gne2

    | Quot(e1, e2) ->
      let t, gne1 = (expr_to_gne t e1 input memory ctx_msg vl) in
      let t, gne2 = (expr_to_gne t e2 input memory ctx_msg vl) in
      t, Gne.div gne1 gne2

    | Mod(e1, e2)  ->
      let t, gne1 = (expr_to_gne t e1 input memory ctx_msg vl) in
      let t, gne2 = (expr_to_gne t e2 input memory ctx_msg vl) in
      t, Gne.modulo gne1 gne2

    | Div(e1, e2) ->
      let t, gne1 = (expr_to_gne t e1 input memory ctx_msg vl) in
      let t, gne2 = (expr_to_gne t e2 input memory ctx_msg vl) in
      t, Gne.div gne1 gne2

    | Ivar(v) ->
      let str = (Var.name v) in
      ( match (lookup input memory ctx_msg vl v) with
          Some(N(I(i))) ->
          (t, Gne.make (Ne.make "" (I i)) true)
        | None ->
          (match (Var.alias v) with
             Some (Numer(ea)) ->
             expr_to_gne t ea input memory ctx_msg vl
           | Some (Formu(f)) ->
             output_msg "\n*** Type error.\n*** ";
             output_msg (formula_to_string f) ;
             output_msg " ougth to be a numeric expression.\n";
             exit 2
           | Some (Liste _l) ->
             assert false
           | None ->
             (t, Gne.make (Ne.make str (I(Num.num_of_int 1))) false)
          )
        | Some(N(F(f))) ->
          output_msg "\n*** Warning : type error, ";
          output_msg ((string_of_float f)
                      ^ " is a float, but an int is expected.\n");
          let i = int_of_float f in
          (t, Gne.make (Ne.make "" (I (Num.num_of_int i))) true
          ) 
        | Some(B(f)) ->
          output_msg "\n*** Warning : type error, ";
          output_msg ((string_of_bool f)
                      ^ " is a bool, but an int is expected. " ^
                      "Continuing with 0.\n");
          (t, Gne.make
             (Ne.make "" (I (Num.num_of_int 0)))
             true
          )
      )

    | Fvar(v) ->
      let str = (Var.name v) in
      ( match (lookup input memory ctx_msg vl v) with
          Some(N(F(f))) ->
          (t, Gne.make (Ne.make "" (F(f))) true)
        | None ->
          (match (Var.alias v) with
             Some (Numer(ea)) -> expr_to_gne t ea input memory ctx_msg vl
           | Some (Formu(f)) ->
             output_msg "\n*** Type error.\n*** ";
             output_msg (formula_to_string f) ;
             output_msg " ougth to be a numeric expression.\n";
             assert false
           | Some (Liste _l) -> assert false
           | None ->
             (t, Gne.make (Ne.make str (F(1.))) false)
          )
        | Some(N(I(i))) ->
          let f = Num.float_of_num i in
          output_msg "\n*** Type error, ";
          output_msg ((Num.string_of_num i)
                      ^ " is an int, but a float is expected. I convert it to '"^
                      (string_of_float f)^"'\n");
          assert false

        | Some(B(f)) ->
          let ff = if f then 0.0 else 1.0 in
          output_msg "\n*** Type error, ";
          output_msg ((string_of_bool f)
                      ^ " is a bool, but a float is expected. I convert it to '"^
                      (string_of_float ff)^"'\n");
          assert false
      )

    | Ival(i) ->
      (t, Gne.make
         (Ne.make "" (I(i)))
         false
      )

    | Fval(f) ->
      (t,  Gne.make
         (Ne.make "" (F(f)))
         false
      )

    | FFC(id, cfunc, _type_args, _lib_name, args) ->
      let t, evaluated_args = eval_ezdl_args t input memory ctx_msg vl id args in
      let _ = if vl > 1 then (
          print_string ("\t" ^ id ^ " "^ 
                        (List.fold_left 
                           (fun acc t  -> acc ^ " " ^(Ezdl.carg_to_string t)) 
                           "" evaluated_args));
          flush stdout;
        )
      in
      let res_call = Ezdl.cargs2f cfunc evaluated_args in
      let _ = if vl > 1 then (
          print_string (" = " ^ (string_of_float res_call) ^ "\n");
          flush stdout;
        )
      in
      (t,  Gne.make
         (Ne.make "" (F(res_call)))
         true (* As C functions may perform side-effects, we need 
                 to recompute the result at each step !
                     not only an heuristic here...
              *)
      )
    | IFC(id, cfunc, _type_args, _lib_name, args) ->
      let t, evaluated_args = eval_ezdl_args t input memory ctx_msg vl id args in
      let _ = if vl > 1 then (
          print_string ("\t" ^ id ^ " "^ 
                        (List.fold_left 
                           (fun acc t  -> acc ^ " " ^(Ezdl.carg_to_string t)) 
                           "" 
                           evaluated_args));
          flush stdout;
        )
      in
      let res_call = Ezdl.cargs2i cfunc evaluated_args in
      let _ = if vl > 1 then (
          print_string (" = " ^ (string_of_int res_call) ^ "\n");
          flush stdout;
        )
      in
      (t, Gne.make
         (Ne.make "" (I(Num.num_of_int res_call)))
         true
      )
    (* Those one are hard-coded to make things simpler and faster for Lutin. *)
    | Gcont(a1,a2,a3) -> 
      let (t, a1') = eval_int_arg t input memory ctx_msg vl "gauss_continue" a1 in
      let (t, a2') = eval_int_arg t input memory ctx_msg vl "gauss_continue" a2 in
      let (t, a3') = eval_int_arg t input memory ctx_msg vl "gauss_continue" a3 in
      let i = LutinUtils.gauss_continue a1' a2' a3' in
      (t, Gne.make
         (Ne.make "" (I(Num.num_of_int i)))
         true
      )
    | Gstop(a1,a2,a3) ->
      let (t, a1') = eval_int_arg t input memory ctx_msg vl "gauss_stop" a1 in
      let (t, a2') = eval_int_arg t input memory ctx_msg vl "gauss_stop" a2 in
      let (t, a3') = eval_int_arg t input memory ctx_msg vl "gauss_stop" a3 in
      let i = LutinUtils.gauss_stop a1' a2' a3' in
      (t, Gne.make
         (Ne.make "" (I(Num.num_of_int i)))
         true
      )
    | Icont(a1,a2,a3) ->
      let (t, a1') = eval_int_arg t input memory ctx_msg vl "interval_continue" a1 in
      let (t, a2') = eval_int_arg t input memory ctx_msg vl "interval_continue" a2 in
      let (t, a3') = eval_int_arg t input memory ctx_msg vl "interval_continue" a3 in
      let i = LutinUtils.interval_continue a1' a2' a3' in
      (t, Gne.make
         (Ne.make "" (I(Num.num_of_int i)))
         true
      )
    | Istop(a1,a2,a3) ->
      let (t, a1') = eval_int_arg t input memory ctx_msg vl "interval_stop" a1 in
      let (t, a2') = eval_int_arg t input memory ctx_msg vl "interval_stop" a2 in
      let (t, a3') = eval_int_arg t input memory ctx_msg vl "interval_stop" a3 in
      let i = LutinUtils.interval_stop a1' a2' a3' in
      (t, Gne.make
         (Ne.make "" (I(Num.num_of_int i)))
         true
      )
    | Ite(f, e1, e2) ->
      let (t, bdd, depf) = translate_do t input memory ctx_msg vl f in
      let t, gne_t = (expr_to_gne t e1 input memory ctx_msg vl) in
      let t, gne_e = (expr_to_gne t e2 input memory ctx_msg vl) in
      t, Gne.of_ite bdd depf gne_t gne_e

and (eval_ezdl_args : t -> Var.env_in -> Var.env -> string -> int -> string -> Exp.t list
     -> t * Ezdl.carg list) =
  fun t input memory ctx_msg vl id args ->
    let t,l =
      List.fold_left
        (fun (t,acc) arg -> 
           match arg with
             Formu _bool_expr -> assert false
           | Numer num_expr -> 
             let t, gne = num_to_gne t input memory ctx_msg vl num_expr in
             (match Gne.get_constant gne with
              | None ->
                let errmsg = 
                  "*** Error when calling " ^ id ^ ". " ^
                  (Exp.num_to_string num_expr) ^ 
                  " should be bound (i.e., it should depend " ^ 
                  "only one inputs and memories).\n"
                in
                failwith errmsg
              | Some (I i) -> t, (Ezdl.Int_carg (Util.int_of_num i))::acc
              | Some (F f) -> t, (Ezdl.Double_carg f)::acc
             )
           | Liste _ -> assert false
        )
        (t,[])
        args 
    in t, List.rev l

and (eval_int_arg : t -> Var.env_in -> Var.env -> string -> int -> string -> Exp.num ->
     t * int) =
  fun t input memory ctx_msg vl id num_expr ->
    let t,n = (eval_num_arg t input memory ctx_msg vl id num_expr) in
    t, Util.int_of_num n

and (eval_num_arg : t -> Var.env_in -> Var.env -> string -> int -> string -> Exp.num ->
     t * Num.num) =
  fun t input memory ctx_msg vl id num_expr ->
    let t, gne = num_to_gne t input memory ctx_msg vl num_expr in
    (match Gne.get_constant gne with
     | Some (I i) ->  t, i
     | Some (F _f) -> 
       let errmsg = 
         "*** Error when calling " ^ id ^ ". " ^
         (Exp.num_to_string num_expr) ^ " should be an integer.\n"
       in
       failwith errmsg
     | _ ->
       let errmsg = 
         "*** Error when calling " ^ id ^ ". " ^
         (Exp.num_to_string num_expr) ^ 
         " should be bound (i.e., it should depend " ^ 
         "only one inputs and memories).\n"
       in
       failwith errmsg
    )

and
  (gne_to_bdd : t -> Gne.t -> comp -> t * Bdd.t * bool) =
  fun t gne cmp ->
    (* Use [cmp] to compare [gne] with 0 and returns the
        corresponding formula.  E.g., if [gne] is bounded to
        [e1 -> c1; e2 -> c2], then [gne_to_bdd gne SupZero] returns
        (the bdd corresponding to) the formula [(c1 and (e1 > 0)) or
        (c2 and (e2 > 0))] 

        Also returns a flag that is true iff [e] depends on pre or
        input vars.
    *)
    match cmp with
      SupZero ->
      ( Gne.fold
          (fun nexpr (c, dep) (t, acc, dep_acc) ->
             let new_dep = dep || dep_acc
             and t, bdd =
               if
                 Ne.is_a_constant nexpr
               then
                 let cst = Ne.find "" nexpr in
                 match cst with
                   Some(I(i)) ->
                   if Num.ge_num i (Num.num_of_int 0)
                   then (t, Bdd.dtrue t.bdd_manager)
                   else (t, Bdd.dfalse t.bdd_manager)
                 | Some(F(f)) ->
                   if f > 0.
                   then (t, Bdd.dtrue t.bdd_manager)
                   else (t, Bdd.dfalse t.bdd_manager)
                 | None ->
                   (t, Bdd.dfalse t.bdd_manager)
               else
                 let t, i = (linear_constraint_to_index t (Ineq(GZ(nexpr))) dep) in
                 t, Bdd.ithvar i                   
             in
             (t, Bdd.dor (Bdd.dand c bdd) acc, new_dep)
          )
          gne
          (t, (Bdd.dfalse t.bdd_manager), false)
      )
    | SupEqZero ->
      ( Gne.fold
          (fun nexpr (c, dep) (t, acc, dep_acc) ->
             let new_dep = dep || dep_acc
             and t, bdd =
               if Ne.is_a_constant nexpr
               then
                 let cst = Ne.find "" nexpr in
                 match cst with
                   Some(I(i)) ->
                   if Num.ge_num i (Num.num_of_int 0) 
                   then (t, Bdd.dtrue t.bdd_manager)
                   else (t, Bdd.dfalse t.bdd_manager)
                 | Some(F(f)) ->
                   if f >= 0.
                   then (t, Bdd.dtrue t.bdd_manager)
                   else (t, Bdd.dfalse t.bdd_manager)
                 | None ->
                   (t, Bdd.dtrue t.bdd_manager)
               else
                 let t, i = linear_constraint_to_index t (Ineq(GeqZ(nexpr))) dep in
                 t, Bdd.ithvar i                   
             in
             (t, Bdd.dor (Bdd.dand c bdd) acc, new_dep)
          )
          gne
          (t, (Bdd.dfalse t.bdd_manager), false)
      )
    | EqZero ->
      ( Gne.fold
          (fun nexpr (c, dep) (t, acc, dep_acc) ->
             let new_dep = dep || dep_acc
             and t, bdd =
               if Ne.is_a_constant nexpr
               then
                 let cst = Ne.find "" nexpr in
                 match cst with
                   Some(I(i)) ->
                   if Num.eq_num i (Num.num_of_int 0) 
                   then (t, Bdd.dtrue t.bdd_manager)
                   else (t, Bdd.dfalse t.bdd_manager)
                 | Some(F(f)) ->
                   if f = 0.
                   then (t, Bdd.dtrue t.bdd_manager)
                   else (t, Bdd.dfalse t.bdd_manager)
                 | None ->
                   (t, Bdd.dtrue t.bdd_manager)
               else
                 let t,i = linear_constraint_to_index t (EqZ(nexpr)) dep in
                 t, Bdd.ithvar i
                   
             in
             (t, Bdd.dor (Bdd.dand c bdd) acc, new_dep)
          )
          gne
          (t, (Bdd.dfalse t.bdd_manager), false)
      )

(* exported *)
let (f : t -> Var.env_in -> Var.env -> string -> int -> Exp.formula -> t * Bdd.t) =
  fun t input memory ctx_msg vl f ->
    let x1, x2, _ = translate_do t input memory ctx_msg vl f in
      if vl > 1 then (
        Printf.eprintf ">>> supp(Formula_to_bdd.f(%s)) = [%s] \n %s\n"
          ((formula_to_string f)) 
          (String.concat ","
             (List.map string_of_int (Bdd.list_of_support  (Bdd.support x2))))
        (tbl_to_string t); 
        flush stderr
      );
    x1, x2

let (eval_int_expr: t -> Exp.num -> string -> Var.env_in -> Var.env  ->
     int -> int option) =
  fun t e msg input mem vl -> 
    let _t, gne = num_to_gne t input mem msg vl e in
    match Gne.get_constant gne with
      None -> None
    | Some (Value.I i) -> Some (Util.int_of_num i)

    | Some (Value.F _) -> assert false


(***********************************************************************)
