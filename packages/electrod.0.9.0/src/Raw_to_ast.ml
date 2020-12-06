(*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2020 ONERA
 * Authors: Julien Brunel (ONERA), David Chemouil (ONERA)
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * 
 * SPDX-License-Identifier: MPL-2.0
 * License-Filename: LICENSE.md
 ******************************************************************************)

open Containers
open Raw
module TS = Tuple_set

(*******************************************************************************
 *  Domain computation
 *******************************************************************************)

let split_indexed_id infile id =
  let name = Raw_ident.basename id in
  match String.Split.right ~by:"$" name with
  | None ->
      assert false (* comes from the lexer so cannot be None*)
  | Some (left, right) ->
      let rightnum =
        try int_of_string right with
        | Failure _ ->
            Msg.Fatal.wrong_suffix (fun args -> args infile id)
      in
      (left, rightnum)


(* check whether [atoms] contains duplicate atoms, warn about them and return the de-duplicated list *)
let check_duplicate_atoms infile atoms =
  (* sort and remove duplicates *)
  let dedup = List.sort_uniq ~cmp:Atom.compare atoms in
  (* check whether we lost elements by doing this*)
  if List.length atoms > List.length dedup
  then
    Msg.Warn.univ_duplicate_atoms (fun args ->
        args infile (List.sort Atom.compare atoms) dedup);
  dedup


let interval_to_atoms infile (first, last) =
  let firstbasename, firstnum = split_indexed_id infile first in
  let lastbasename, lastnum = split_indexed_id infile last in
  if String.compare firstbasename lastbasename <> 0
  then Msg.Fatal.different_prefixes (fun args -> args infile first last)
  else if firstnum > lastnum
  then Msg.Fatal.not_an_interval (fun args -> args infile first last)
  else
    let open List in
    firstnum -- lastnum
    |> map (fun num -> Atom.atom @@ Printf.sprintf "%s$%d" firstbasename num)


let compute_univ infile raw_univ =
  let open List in
  let atoms =
    flat_map
      (function
        | UIntvl intvl ->
            interval_to_atoms infile intvl
        | UPlain id ->
            [ Atom.of_raw_ident id ])
      raw_univ
  in
  let dedup = check_duplicate_atoms infile atoms in
  let bound = List.map Tuple.tuple1 dedup |> TS.of_tuples in
  Relation.(const Name.univ 1 @@ Scope.exact bound)


(* 1 = arity *)

(* returns a list of tuples (possibly 1-tuples corresponding to plain atoms) *)
let compute_tuples infile domain = function
  (* a list of  1-tuples (coming from indexed id's) *)
  | EIntvl intvl ->
      (* Msg.debug (fun m -> m "Raw_to_ast.compute_tuples:EIntvl"); *)
      let atoms = interval_to_atoms infile intvl in
      let absent =
        (* compute 1-tuples/atoms absent from univ, if there are *)
        List.flat_map
          (fun t ->
            if not @@ TS.mem (Tuple.tuple1 t) @@ Domain.univ_atoms domain
            then [ t ]
            else [])
          atoms
      in
      ( if not @@ List.is_empty absent
      then
        Msg.Fatal.undeclared_atoms
        @@ fun args ->
        args
          infile
          (Location.span @@ Pair.map_same Raw_ident.location intvl)
          absent );
      let dedup = check_duplicate_atoms infile atoms in
      List.map Tuple.tuple1 dedup
  | ETuple [] ->
      assert false
  (* grammatically impossible *)
  (* a single n-ary tuple *)
  | ETuple ids ->
      (* Msg.debug (fun m -> m "Raw_to_ast.compute_tuples:ETuple"); *)
      let atoms = List.map (fun id -> Raw_ident.basename id |> Atom.atom) ids in
      (* to check if all atoms in the tuple are in univ, we do as if every atom
         was a 1-tuple and then check whether this 1-tuple is indeed in univ *)
      let absent =
        (* compute 1-tuples/atoms absent from univ, if there are *)
        List.flat_map
          (fun t ->
            if not @@ TS.mem (Tuple.tuple1 t) @@ Domain.univ_atoms domain
            then [ t ]
            else [])
          atoms
      in
      ( if not @@ List.is_empty absent
      then
        Msg.Fatal.undeclared_atoms
        @@ fun args ->
        args
          infile
          ( Location.span
          @@ Pair.map_same Raw_ident.location List.(hd ids, hd @@ last 1 ids) )
          absent );
      [ Tuple.of_list1 atoms ]


let check_tuples_arities_and_duplicates infile id = function
  | [] ->
      TS.empty
  | t :: ts as tuples ->
      let ar = Tuple.arity t in
      (* List.iter (fun t -> Msg.debug (fun m -> m "ar(%a) = %d" Tuple.pp t ar)) tuples; *)
      if List.exists (fun t2 -> Tuple.arity t2 <> ar) ts
      then Msg.Fatal.incompatible_arities (fun args -> args infile id);
      TS.of_tuples tuples


(* [`Inf] and [`Sup] tell whether we are computing a lower of upper bound:
   this is important as a bound may be defined out of other ones, so we
   should know whether we need the lower or upper bound of the relations
   referred to. The variants are in an [option] which is set to [None] if
   the scope is exact (in which case, the variants are of no use); [Some]
   otherwise.

   We also pass the [id] of the concerned relation (useful for error message). *)
let compute_bound infile domain (which : [ `Inf | `Sup | `Exact ]) id raw_bound
    =
  let open Relation in
  let open Scope in
  let rec walk = function
    | BUniv ->
        (* Msg.debug (fun m -> m "Raw_to_ast.compute_bound:BUniv"); *)
        Domain.univ_atoms domain
    | BRef ref_id ->
      ( (* Msg.debug (fun m -> m "Raw_to_ast.compute_bound:BRef"); *)
      match Domain.get (Name.of_raw_ident ref_id) domain with
      | None ->
          Msg.Fatal.undeclared_id (fun args -> args infile ref_id)
      | Some rel ->
        ( match rel with
        | Const { scope = Exact b; _ } when TS.inferred_arity b = 1 ->
            b
        | Const { scope = Inexact _ as sc; _ } ->
            let sup = Scope.sup sc in
            if TS.inferred_arity sup = 1
            then
              match which with
              | `Inf ->
                  Scope.inf sc
              | `Sup ->
                  sup
              | `Exact ->
                  Msg.Fatal.inexact_ref_used_in_exact_scope
                  @@ fun args -> args infile id ref_id
            else
              Msg.Fatal.should_denote_a_constant_set
              @@ fun args -> args infile ref_id
        | Const { scope = Exact _; _ } | Var _ ->
            Msg.Fatal.should_denote_a_constant_set
            @@ fun args -> args infile ref_id ) )
    | BProd (_, Some _, _) ->
        Msg.Fatal.no_multiplicity_allowed_here (fun args -> args infile id)
    | BProd (rb1, None, rb2) ->
        (* Msg.debug (fun m -> m "Raw_to_ast.compute_bound:BProd"); *)
        let b1 = walk rb1 in
        let b2 = walk rb2 in
        TS.product b1 b2
    | BUnion (rb1, rb2) ->
        (* Msg.debug (fun m -> m "Raw_to_ast.compute_bound:BUnion"); *)
        let b1 = walk rb1 in
        let b2 = walk rb2 in
        if TS.inferred_arity b1 = TS.inferred_arity b2
        then TS.union b1 b2
        else Msg.Fatal.incompatible_arities @@ fun args -> args infile id
    | BElts elts ->
        (* Msg.debug (fun m -> m "Raw_to_ast.compute_bound:BElts"); *)
        let tuples = List.flat_map (compute_tuples infile domain) elts in
        let bnd = check_tuples_arities_and_duplicates infile id tuples in
        if TS.size bnd <> List.length tuples
        then
          Msg.Warn.duplicate_elements (fun args ->
              args infile id which TS.pp bnd);
        bnd
  in
  walk raw_bound


let compute_scope infile domain id = function
  | SExact (BProd (_, Some _, _)) ->
      Msg.Fatal.multiplicity_only_in_a_sup (fun args -> args infile id)
  | SExact raw_b ->
      (* Msg.debug (fun m -> m "Raw_to_ast.compute_scope:SExact"); *)
      Scope.exact @@ compute_bound infile domain `Exact id raw_b
  (* handle when we have a partial/total "function"
     with empty domain: r : {} (l)one (sthg) *)
  | SInexact (raw_inf, Some function_type, raw_sup) ->
      let inf = compute_bound infile domain `Inf id raw_inf in
      if not @@ TS.is_empty inf
      then Msg.Fatal.inf_must_be_empty @@ fun args -> args infile id
      else
        let sup = compute_bound infile domain `Sup id raw_sup in
        ( match function_type with
        | `Lone ->
            Scope.(fun s -> inexact @@ partial_function 0 s) sup
        | `One ->
            Scope.(fun s -> inexact @@ total_function 0 s) sup )
  (* handle when we have a partial/total "function" with nonempty domain: r : {}
     (sthg -> (l)one sthg)

     REMARK: we rely on -> being declared *left associative* in the parser!, so
     that the multiplicity indeed appears on the toplevel arrow of a scope
     declaration! *)
  | SInexact (raw_inf, None, BProd (rb1, Some function_type, rb2)) ->
      let inf = compute_bound infile domain `Inf id raw_inf in
      if not @@ TS.is_empty inf
      then Msg.Fatal.inf_must_be_empty @@ fun args -> args infile id
      else
        let sup1 = compute_bound infile domain `Sup id rb1 in
        let sup2 = compute_bound infile domain `Sup id rb2 in
        let dom_ar = TS.inferred_arity sup1 in
        let sup = TS.product sup1 sup2 in
        ( match function_type with
        | `Lone ->
            Scope.(fun s -> inexact @@ partial_function dom_ar s) sup
        | `One ->
            Scope.(fun s -> inexact @@ total_function dom_ar s) sup )
  | SInexact (raw_inf, None, raw_sup) ->
      (* Msg.debug (fun m -> m "Raw_to_ast.compute_scope:SInexact"); *)
      let inf = compute_bound infile domain `Inf id raw_inf in
      let sup = compute_bound infile domain `Sup id raw_sup in
      let ar_inf = TS.inferred_arity inf in
      let ar_sup = TS.inferred_arity sup in
      if ar_inf <> ar_sup && not (TS.is_empty inf)
      then Msg.Fatal.incompatible_arities (fun args -> args infile id);
      if not @@ TS.subset inf sup
      then Msg.Fatal.inf_not_in_sup (fun args -> args infile id TS.pp inf sup);
      if TS.is_empty sup
      then Msg.Warn.empty_scope_declared (fun args -> args infile id);
      if TS.equal inf sup
      then Scope.exact sup
      else Scope.(fun i s -> inexact @@ plain_relation i s) inf sup


let check_name infile id domain =
  let name = Name.of_raw_ident id in
  if Domain.mem name domain
  then Msg.Fatal.rel_name_already_used @@ fun args -> args infile id


let decide_arity infile id specified_arity computed_arity =
  match specified_arity with
  | None when computed_arity < 1 ->
      Msg.Fatal.cannot_decide_arity (fun args -> args infile id)
  | Some ar when ar <> computed_arity && computed_arity <> 0 ->
      Msg.Fatal.specified_computed_arities_discrepancy (fun args ->
          args infile id ar computed_arity)
  | None ->
      computed_arity
  | Some ar ->
      ar


let compute_decl infile domain = function
  | DConst (id, specified_arity, raw_scope) ->
      (* Msg.debug (fun m -> m "Raw_to_ast.compute_decl:DConst"); *)
      check_name infile id domain;
      let scope = compute_scope infile domain id raw_scope in
      (* deal with posisble mismatch btw the computed arity and that declared *)
      let computed_arity = Scope.inferred_arity scope in
      let arity = decide_arity infile id specified_arity computed_arity in
      Relation.const (Name.of_raw_ident id) arity scope
  | DVar (id, specified_arity, init, fby) ->
      (* Msg.debug (fun m -> m "Raw_to_ast.compute_decl:DVar"); *)
      check_name infile id domain;
      let init_scope = compute_scope infile domain id init in
      let fby_scope = CCOpt.map (compute_scope infile domain id) fby in
      let init_arity = Scope.inferred_arity init_scope in
      let computed_arity =
        match CCOpt.map Scope.inferred_arity fby_scope with
        | None | Some 0 ->
            init_arity (* 0 : arity cannot be inferred *)
        | Some ar when ar <> init_arity && init_arity <> 0 ->
            Msg.Fatal.init_and_fby_incompatible_arities (fun args ->
                args infile id init_arity ar)
        | Some ar ->
            ar
      in
      let arity = decide_arity infile id specified_arity computed_arity in
      Relation.var (Name.of_raw_ident id) arity init_scope fby_scope


let compute_domain (pb : Raw.raw_problem) =
  let univ = compute_univ pb.file pb.raw_univ in
  let univ_ts = Relation.must univ in
  (* corresponding tuple set *)
  let iden = Relation.const Name.iden 2 @@ Scope.exact @@ TS.diagonal univ_ts in
  let init =
    Domain.add Name.univ univ Domain.empty |> Domain.add Name.iden iden
  in
  (* updating the domain, given a previous domain and a raw_decl *)
  let update dom decl =
    let name = Name.of_raw_ident @@ Raw.decl_id decl in
    let rel = compute_decl pb.file dom decl in
    Domain.add name rel dom
    (* |> tap Msg.debug *)
    (*   (fun m -> m "Raw_to_ast.compute_domain:update add %a ⇒ %a" *)
    (*               Name.pp name (Fmtc.hbox @@ Domain.pp) newdom) *)
  in
  List.fold_left update init pb.raw_decls


(*******************************************************************************
 *  Compute instances and check they comply with the respective relations.
 *******************************************************************************)

let check_assignment_in_scope infile domain id tupleset =
  let name = Name.of_raw_ident id in
  match Domain.get name domain with
  | None ->
      Msg.Fatal.undeclared_id (fun args -> args infile id)
  | Some (Relation.Var _) ->
      Msg.Fatal.instance_is_var (fun args -> args infile id)
  | Some (Relation.Const { scope; _ })
    when not @@ Scope.included_in tupleset scope ->
      Msg.Fatal.instance_not_in_scope (fun args -> args infile id)
  | Some (Relation.Const _) ->
      ()


(* [domain]: already-computed domain *)
let compute_instances domain (pb : Raw.raw_problem) =
  let compute_assignment (id, raw_tuples) =
    let name = Name.of_raw_ident id in
    let tupleset =
      raw_tuples
      |> List.map CCFun.(List.map Atom.of_raw_ident %> Tuple.of_list1)
      |> CCFun.tap (check_tuples_arities_and_duplicates pb.file id)
      |> TS.of_tuples
      |> CCFun.tap (check_assignment_in_scope pb.file domain id)
    in
    (name, tupleset)
  in
  List.fold_left
    (fun acc asgn ->
      compute_assignment asgn
      |> fun (n, ts) ->
      if Instance.mem n acc
      then
        Msg.Fatal.instance_already_declared (fun args ->
            args pb.file @@ fst asgn)
      else Instance.add n ts acc)
    Instance.empty
    pb.raw_inst


(******************************************************************************
 *  Compute the symmetries.
 *****************************************************************************)
(* Compute the symmetry contraints *)
let compute_symmetries (pb : Raw.raw_problem) =
  let compute_single_sym_term (id, raw_tuple) =
    let name = Name.of_raw_ident id in
    let tuple = Tuple.of_list1 @@ List.map Atom.of_raw_ident raw_tuple in
    (name, tuple)
  in
  let compute_single_sym
      (sym :
        (Raw_ident.t * Raw.raw_tuple) list * (Raw_ident.t * Raw.raw_tuple) list)
      =
    match sym with
    | [], [] ->
        Symmetry.make [] []
    (* impossible case: only one side of the symmetry is empty *)
    | [], _ | _, [] ->
        assert false
    | l1, l2 ->
        let len1 = List.length l1 in
        let len2 = List.length l2 in
        if len1 <> len2
        then
          let id, _ = List.hd l1 in
          Msg.Fatal.symmetry_wrongly_defined (fun args -> args pb.file id)
        else
          Symmetry.make
            (List.map compute_single_sym_term l1)
            (List.map compute_single_sym_term l2)
  in
  List.map compute_single_sym pb.raw_syms


(*******************************************************************************
 *  Walking along raw goals to get variables and relation names out of raw_idents
 *******************************************************************************)

let refine_identifiers raw_pb =
  let open Gen_goal in
  let rec walk_fml ctx fml =
    let ctx2, f = walk_prim_fml ctx fml.prim_fml in
    (ctx2, { fml with prim_fml = f })
  and walk_prim_fml ctx = function
    | Quant (q, sim_bindings, blk) ->
        let ctx2, sim_bindings2 = walk_sim_bindings ctx sim_bindings in
        let _, blk2 = walk_block ctx2 blk in
        (ctx, quant q sim_bindings2 blk2)
    | True ->
        (ctx, true_)
    | False ->
        (ctx, false_)
    | Block b ->
        (ctx, Pair.snd_map block (walk_block ctx b))
    | LUn (op, fml) ->
        (ctx, Pair.snd_map (lunary op) (walk_fml ctx fml))
    | LBin (f1, op, f2) ->
        (ctx, lbinary (snd @@ walk_fml ctx f1) op (snd @@ walk_fml ctx f2))
    | Qual (q, r) ->
        (ctx, qual q @@ walk_exp ctx r)
    | RComp (e1, op, e2) ->
        (ctx, rcomp (walk_exp ctx e1) op (walk_exp ctx e2))
    | IComp (e1, op, e2) ->
        (ctx, icomp (walk_iexp ctx e1) op (walk_iexp ctx e2))
    | FIte (c, t, e) ->
        ( ctx
        , fite
            (snd @@ walk_fml ctx c)
            (snd @@ walk_fml ctx t)
            (snd @@ walk_fml ctx e) )
    | Let (bindings, blk) ->
        let ctx2, bindings2 = walk_bindings ctx bindings in
        let _, blk2 = walk_block ctx2 blk in
        (ctx, let_ bindings2 blk2)
  and walk_bindings ctx = function
    | [] ->
        (ctx, [])
    | b :: bs ->
        let ctx2, b2 = walk_binding ctx b in
        let ctx3, bs2 = walk_bindings ctx2 bs in
        (ctx3, b2 :: bs2)
  and walk_binding ctx (v, exp) =
    let exp2 = walk_exp ctx exp in
    let var = Var.fresh_of_raw_ident v in
    ((v, Ast.var_ident var) :: ctx, (Ast.bound_var var, exp2))
  and walk_sim_bindings ctx = function
    | [] ->
        (ctx, [])
    | sb :: sbs ->
        let ctx2, sb2 = walk_sim_binding ctx sb in
        let ctx3, sbs2 = walk_sim_bindings ctx2 sbs in
        (ctx3, sb2 :: sbs2)
  and walk_sim_binding ctx (disj, vs, exp) =
    let disj2 =
      if disj && List.length vs = 1
      then (
        Msg.Warn.disj_with_only_one_variable (fun args ->
            args raw_pb.file (List.hd vs));
        false )
      else disj
    in
    let exp2 = walk_exp ctx exp in
    let bvars =
      List.map (fun v -> Ast.bound_var (Var.fresh (Raw_ident.basename v))) vs
    in
    let vars = List.map Ast.var_ident_of_bound_var bvars in
    (List.(combine vs vars |> rev) @ ctx, (disj2, bvars, exp2))
  and walk_block ctx blk =
    (ctx, List.map (fun fml -> snd @@ walk_fml ctx fml) blk)
  and walk_exp ctx exp = { exp with prim_exp = walk_prim_exp ctx exp.prim_exp }
  and walk_prim_exp ctx = function
    | Ident id ->
      ( try ident @@ CCList.Assoc.get_exn ~eq:Raw_ident.eq_name id ctx with
      | Not_found ->
          Msg.Fatal.undeclared_id @@ fun args -> args raw_pb.file id )
    | None_ ->
        none
    | Univ ->
        univ
    | Iden ->
        iden
    | RUn (op, e) ->
        runary op @@ walk_exp ctx e
    | RBin (e1, op, e2) ->
        rbinary (walk_exp ctx e1) op (walk_exp ctx e2)
    | RIte (c, t, e) ->
        rite (snd @@ walk_fml ctx c) (walk_exp ctx t) (walk_exp ctx e)
    | BoxJoin (e, args) ->
        boxjoin (walk_exp ctx e) @@ List.map (walk_exp ctx) args
    | Prime e ->
        prime (walk_exp ctx e)
    | Compr (sim_bindings, blk) ->
        let ctx2, sim_bindings2 = walk_sim_bindings ctx sim_bindings in
        let _, blk2 = walk_block ctx2 blk in
        compr sim_bindings2 blk2
  and walk_iexp ctx iexp =
    { iexp with prim_iexp = walk_prim_iexp ctx iexp.prim_iexp }
  and walk_prim_iexp ctx = function
    | Num n ->
        num n
    | Card e ->
        card @@ walk_exp ctx e
    | IUn (op, e) ->
        iunary op @@ walk_iexp ctx e
    | IBin (e1, op, e2) ->
        ibinary (walk_iexp ctx e1) op (walk_iexp ctx e2)
  in
  (* initial context is made of relation names declared in the domain (+ univ) *)
  let init_ctx =
    List.map
      (fun decl ->
        Pair.dup_map (fun id -> Ast.name_ident (Name.of_raw_ident id))
        @@ Raw.decl_id decl)
      raw_pb.raw_decls
    @ [ ( Raw_ident.ident "univ" Lexing.dummy_pos Lexing.dummy_pos
        , Ast.name_ident Name.univ )
      ]
  in
  let walk_goal = function
    | Run (fml, expect) ->
        run (List.map Fun.(snd % walk_fml init_ctx) fml) expect
  in
  let walk_invariants invs = snd @@ walk_block init_ctx invs in
  (walk_invariants raw_pb.raw_invar, walk_goal raw_pb.raw_goal)


(*******************************************************************************
 *  Check arities 
 *******************************************************************************)

(* computes the arity of a join *)
let join_arity ar1 ar2 =
  match (ar1, ar2) with
  | Some a1, Some a2 ->
      let res = a1 + a2 - 2 in
      if res > 0 then Some res else None
  | Some _, None | None, Some _ | None, None ->
      None


let str_exp = Fmtc.to_to_string (Fmtc.hbox2 Ast.pp_exp)

let compute_arities elo =
  let open Ast in
  let open Gen_goal in
  (* ctx is a map from identifiers to their arity  *)
  let rec walk_fml ctx fml =
    { fml with prim_fml = walk_prim_fml ctx fml.prim_fml }
  and walk_prim_fml ctx = function
    | (True | False) as b ->
        b
    | Qual (q, exp) ->
        qual q @@ walk_exp ctx exp
    | RComp (e1, op, e2) ->
        let e1' = walk_exp ctx e1 in
        let e2' = walk_exp ctx e2 in
        let ar1 = e1'.arity in
        let ar2 = e2'.arity in
        if (not @@ Option.equal ( = ) ar1 ar2)
           && Option.is_some ar1
           && Option.is_some ar2
        then
          Msg.Fatal.arity_error (fun args ->
              args
                elo.Ast.file
                e2
                (Fmtc.strf
                   "arity of %s (%a) incompatible with that of %s (%a)"
                   (str_exp e1)
                   Fmtc.(option ~none:(const string "none") int)
                   ar1
                   (str_exp e2)
                   Fmtc.(option ~none:(const string "none") int)
                   ar2))
        else rcomp e1' op e2'
    | IComp (e1, op, e2) ->
        let e1' = walk_iexp ctx e1 in
        let e2' = walk_iexp ctx e2 in
        icomp e1' op e2'
    | LUn (op, fml) ->
        lunary op @@ walk_fml ctx fml
    | LBin (f1, op, f2) ->
        lbinary (walk_fml ctx f1) op (walk_fml ctx f2)
    | Quant (q, sbs, blk) ->
        let sbs', ctx' = walk_sim_bindings ctx sbs in
        quant q sbs' @@ walk_block ctx' blk
    | Let (bindings, blk) ->
        let bindings', ctx' = walk_bindings ctx bindings in
        let_ bindings' @@ walk_block ctx' blk
    | FIte (c, t, e) ->
        fite (walk_fml ctx c) (walk_fml ctx t) (walk_fml ctx e)
    | Block blk ->
        block @@ walk_block ctx blk
  and walk_block ctx blk = List.map (walk_fml ctx) blk
  and walk_bindings ctx = function
    | [] ->
        ([], ctx)
    | (BVar v, exp) :: bs ->
        let exp' = walk_exp ctx exp in
        let ar = exp'.arity in
        let ctx' = ctx#update [ (v, ar) ] in
        let bs', ctx'' = walk_bindings ctx' bs in
        ((bound_var v, exp') :: bs', ctx'')
  and walk_sim_bindings ctx = function
    | [] ->
        ([], ctx)
    | (disj, vs, exp) :: sbs ->
        let exp' = walk_exp ctx exp in
        let ar = exp'.arity in
        let ctx' = ctx#update @@ List.map (fun (BVar v) -> (v, ar)) vs in
        let sbs', ctx'' = walk_sim_bindings ctx' sbs in
        ((disj, vs, exp') :: sbs', ctx'')
  and walk_exp ctx exp =
    match walk_prim_exp ctx exp with
    | Ok exp' ->
        exp'
    | Error msg ->
        Msg.Fatal.arity_error (fun args -> args elo.Ast.file exp msg)
  and return_exp exp ar pe =
    assert (not @@ Option.equal ( = ) ar (Some 0));
    Result.return { exp with arity = ar; prim_exp = pe }
  (* this function returns a [result] to factor the error messages out and also
     to enable to display the expression (i.e [exp], not [prim_exp]) concerned
     by the error*)
  (* IMPORTANT: the function receives an *expression*, not a *primitive* one; in
     order to easily set the mutable fields of the said expression. *)
  and walk_prim_exp ctx exp =
    match exp.prim_exp with
    | None_ ->
        return_exp exp None none
    | Univ ->
        let arity = ctx#arity (name_ident Name.univ) in
        return_exp exp arity univ
    | Iden ->
        let arity = ctx#arity (name_ident Name.iden) in
        return_exp exp arity iden
    | Ident id ->
        let arity = ctx#arity id in
        return_exp exp arity (ident id)
    | RUn (op, e) ->
        let e' = walk_exp ctx e in
        let ar = e'.arity in
        if not @@ Option.equal ( = ) ar (Some 2)
        then Result.fail "arity should be 2"
        else return_exp exp ar (runary op e')
    | RBin (e1, op, e2) ->
        let e1' = walk_exp ctx e1 in
        let e2' = walk_exp ctx e2 in
        let ar1 = e1'.arity in
        let ar2 = e2'.arity in
        ( match op with
        | Union when Option.equal ( = ) ar1 ar2 || Option.is_none ar2 ->
            return_exp exp ar1 @@ rbinary e1' op e2'
        | Union when Option.is_none ar1 ->
            return_exp exp ar2 @@ rbinary e1' op e2'
        | Union ->
            Result.fail
              (Fmtc.strf
                 "incompatible arities between %s and %s"
                 (str_exp e1')
                 (str_exp e2'))
        | Diff when Option.is_none ar1 ->
            return_exp exp None @@ rbinary e1' op e2'
        | Diff when Option.equal ( = ) ar1 ar2 || Option.is_none ar2 ->
            return_exp exp ar1 @@ rbinary e1' op e2'
        | Diff ->
            Result.fail
              (Fmtc.strf
                 "incompatible arities between %s and %s"
                 (str_exp e1')
                 (str_exp e2'))
        | Inter when Option.is_none ar1 || Option.is_none ar2 ->
            return_exp exp None @@ rbinary e1' op e2'
        | Inter when Option.equal ( = ) ar1 ar2 ->
            return_exp exp ar1 @@ rbinary e1' op e2'
        | Inter ->
            Result.fail
              (Fmtc.strf
                 "incompatible arities between %s and %s"
                 (str_exp e1')
                 (str_exp e2'))
        | Over when Option.equal ( = ) ar1 ar2 ->
            if CCOpt.compare CCInt.compare ar1 (Some 1) <= 0
            then Result.fail (Fmtc.strf "arity of %s is < 2" (str_exp e1'))
            else return_exp exp ar1 @@ rbinary e1' op e2'
        | Over when Option.is_none ar1 ->
            return_exp exp ar2 @@ rbinary e1' op e2'
        | Over when Option.is_none ar2 ->
            return_exp exp ar1 @@ rbinary e1' op e2'
        | Over ->
            Result.fail
              (Fmtc.strf
                 "incompatible arities between %s and %s"
                 (str_exp e1')
                 (str_exp e2'))
        | LProj when Option.is_none ar1 ->
            return_exp exp None @@ rbinary e1' op e2'
        | LProj when Option.equal ( = ) ar1 (Some 1) ->
            return_exp exp ar2 @@ rbinary e1' op e2'
        | LProj ->
            Result.fail "left projection should be on a set"
        | RProj when Option.is_none ar2 ->
            return_exp exp None @@ rbinary e1' op e2'
        | RProj when Option.equal ( = ) ar2 (Some 1) ->
            return_exp exp ar1 @@ rbinary e1' op e2'
        | RProj ->
            Result.fail "right projection should be on a set"
        | Prod ->
          ( match (ar1, ar2) with
          | Some a1, Some a2 ->
              let ar = Some (a1 + a2) in
              return_exp exp ar @@ rbinary e1' op e2'
          | None, _ | _, None ->
              return_exp exp None @@ rbinary e1' op e2' )
        | Join ->
            let ar_join = join_arity ar1 ar2 in
            if Option.is_none ar_join
            then
              Result.fail
              @@ Fmtc.strf
                   "wrong arities for the dot join of %s and %s"
                   (str_exp e1')
                   (str_exp e2')
            else return_exp exp ar_join @@ rbinary e1' op e2' )
    | RIte (c, t, e) ->
        let c' = walk_fml ctx c in
        let t' = walk_exp ctx t in
        let e' = walk_exp ctx e in
        ( match (t'.arity, e'.arity) with
        | Some a1, Some a2 when a1 = a2 ->
            return_exp exp e'.arity (rite c' t' e')
        | Some a, None | None, Some a ->
            return_exp exp (Some a) (rite c' t' e')
        | None, None ->
            return_exp exp None (rite c' t' e')
        | Some _, Some _ ->
            Result.fail
              "incompatible arities in the bodies of 'then' and 'else'" )
    | BoxJoin (call, args) ->
        (* build the iterated "plain" join to get arity/must/sup *)
        let call' = walk_exp ctx call in
        let args' = List.map (walk_exp ctx) args in
        let res =
          List.fold_right
            (fun arg r ->
              Gen_goal.exp
                Option.(map2 ( + ) (pure (-2)) @@ map2 ( + ) arg.arity r.arity)
                Location.(span (arg.exp_loc, r.exp_loc))
              @@ rbinary arg join r)
            args'
            call'
        in
        if Option.is_none res.arity || Option.equal ( = ) res.arity (Some 0)
        then Result.fail "wrong arities for the box join"
        else return_exp exp res.arity @@ boxjoin call' args'
    | Compr (sbs, blk) ->
        let sbs', ctx2 = walk_sim_bindings ctx sbs in
        ( match
            List.(
              flat_map (fun (_, vs, _) ->
                  map (fun v -> ctx2#arity @@ var_ident_of_bound_var v) vs))
              sbs'
          with
        | [] ->
            assert false
        | hd :: tl ->
            let ar = List.fold_left Option.(map2 ( + )) hd tl in
            let blk' = walk_block ctx2 blk in
            return_exp exp ar @@ compr sbs' blk' )
    | Prime e ->
        let e' = walk_exp ctx e in
        return_exp exp e'.arity @@ prime e'
  and walk_iexp ctx iexp =
    { iexp with prim_iexp = walk_prim_iexp ctx iexp.prim_iexp }
  and walk_prim_iexp ctx = function
    | Num n ->
        num n
    | Card exp ->
        card @@ walk_exp ctx exp
    | IUn (op, iexp) ->
        iunary op @@ walk_iexp ctx iexp
    | IBin (iexp1, op, iexp2) ->
        ibinary (walk_iexp ctx iexp1) op (walk_iexp ctx iexp2)
  in
  let init =
    object
      val arities =
        Domain.arities elo.Ast.domain
        |> List.map (fun (n, a) -> (name_ident n, Some a))

      (* |> Fun.tap (fun ars -> *)
      (*       Msg.debug (fun m -> *)
      (*             m "compute_arities.initial arities = %a" *)
      (*               Fmtc.(brackets @@ *)
      (*                     list ~sep:sp *)
      (*                     @@ pair ~sep:(const string "→") *)
      (*                          Ast.pp_ident (option int)) ars )) *)
      val domain = elo.Ast.domain

      method update pairs =
        (* Msg.debug (fun m -> *)
        (*       m "compute_arities.update %a" *)
        (*         Fmtc.(list ~sep:sp @@ pair Var.pp (option int)) pairs); *)
        {<arities = List.map (fun (v, ar) -> (var_ident v, ar)) pairs @ arities>}

      method arity ident = List.Assoc.get_exn ~eq:Ast.equal_ident ident arities
      (* |> Fun.tap (fun ar -> *)
      (*       Msg.debug (fun m -> m "compute_arities.arity %a --> %a" *)
      (*                             Ast.pp_ident ident *)
      (*                             Fmtc.(option int) ar *)
      (*                 )) *)
    end
  in
  let walk_goal ctx = function
    | Run (fmls, expec) ->
        run (List.map (walk_fml ctx) fmls) expec
  in
  Ast.
    { elo with
      invariants = List.map (walk_fml init) elo.invariants
    ; goal = walk_goal init elo.goal
    }


(*******************************************************************************
 *  Declaration of the whole transformation
 *******************************************************************************)

let whole raw_pb =
  let domain = compute_domain raw_pb in
  let syms = compute_symmetries raw_pb in
  let instance = compute_instances domain raw_pb in
  let invars, goal = refine_identifiers raw_pb in
  Ast.make raw_pb.file domain instance syms invars goal |> compute_arities


let transfo = Transfo.make "raw_to_elo" whole

(* temporary *)
