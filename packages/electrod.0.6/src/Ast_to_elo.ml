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

(* This module is supposed to happen after a simplification that addresses: 
   let bindings, *multiple* sim_bindings, quantified expressions, no lone/one quantifier; the input is also supposed to be arity-correct *)

module E = Elo

let convert_arity = function
  | None ->
      0
  | Some n when n > 0 ->
      n
  | Some _ ->
      assert false


(* The "convert_xxx" functions handle conversion from an AST with globally-unique variable names to a hashconsed-De Bruijn-encoded representation. Essentially, we keep a stack of bounded variables (in the current call context) and, if we meet a variable reference, we return its index in the stack. So a variable equal to 0 represents a variable bound by the last binder met.  *)

let get_var x stack =
  match List.find_idx (fun v -> Var.equal x v) stack with
  | None ->
      Format.kasprintf
        failwith
        "%s.get_var: variable %a not found in %a"
        __MODULE__
        Var.pp
        x
        Fmtc.(brackets @@ list ~sep:(sp **> comma) @@ Var.pp)
        stack
  | Some (i, _) ->
      i


let new_env vars stack =
  Msg.debug (fun m ->
      m
        "Ast_to_elo.new_env: stacking %a onto %a"
        Fmt.(brackets @@ list ~sep:comma Ast.pp_var)
        (List.rev vars)
        Fmt.(brackets @@ list ~sep:comma Var.pp)
        stack);
  List.rev_map (function Ast.BVar v -> v) vars @ stack


let rec convert_fml stack ({ prim_fml; _ } : (Ast.var, Ast.ident) Gen_goal.fml)
    =
  match prim_fml with
  | Qual (_, _) ->
      assert false (* simplified *)
  | Let (_, _) ->
      assert false (* simplified *)
  | Quant (_, _ :: _ :: _, _) ->
      assert false (* simplified *)
  | Quant (_, [], _) ->
      assert false (* impossible *)
  | Quant (q, [ (disj, vars, range) ], block) ->
      let range' = convert_exp stack range in
      let block' = convert_block (new_env vars stack) block in
      E.quant (convert_quant q) (disj, List.length vars, range') block'
  | True ->
      E.true_
  | False ->
      E.false_
  | RComp (e1, comp, e2) ->
      E.rcomp
        (convert_exp stack e1)
        (convert_comp_op comp)
        (convert_exp stack e2)
  | IComp (e1, comp, e2) ->
      E.icomp
        (convert_iexp stack e1)
        (convert_icomp_op comp)
        (convert_iexp stack e2)
  | LUn (op, f) ->
      E.lunary (convert_lunop op) (convert_fml stack f)
  | LBin (f1, op, f2) ->
      E.lbinary
        (convert_fml stack f1)
        (convert_lbinop op)
        (convert_fml stack f2)
  | FIte (c, t, e) ->
      E.fite (convert_fml stack c) (convert_fml stack t) (convert_fml stack e)
  | Block fmls ->
      E.block @@ convert_block stack fmls


and convert_block stack fmls = List.map (convert_fml stack) fmls

and convert_quant (q : Gen_goal.quant) =
  match q with
  | One | Lone ->
      assert false (* simplified *)
  | All ->
      E.all
  | Some_ ->
      E.some
  | No ->
      E.no_


and convert_comp_op (comp : Gen_goal.comp_op) =
  match comp with
  | In ->
      E.in_
  | NotIn ->
      E.not_in
  | REq ->
      E.req
  | RNEq ->
      E.rneq


and convert_icomp_op (comp : Gen_goal.icomp_op) =
  match comp with
  | IEq ->
      E.ieq
  | INEq ->
      E.ineq
  | Lt ->
      E.lt
  | Lte ->
      E.lte
  | Gt ->
      E.gt
  | Gte ->
      E.gte


and convert_lunop (op : Gen_goal.lunop) =
  match op with
  | F ->
      E.sometime
  | G ->
      E.always
  | Not ->
      E.not_
  | O ->
      E.once
  | X ->
      E.next
  | H ->
      E.historically
  | P ->
      E.previous


and convert_lbinop (op : Gen_goal.lbinop) =
  match op with
  | And ->
      E.and_
  | Or ->
      E.or_
  | Imp ->
      E.impl
  | Iff ->
      E.iff
  | U ->
      E.until
  | R ->
      E.releases
  | S ->
      E.since


and convert_exp
    stack ({ prim_exp; arity; _ } : (Ast.var, Ast.ident) Gen_goal.exp) =
  let ar = convert_arity arity in
  match prim_exp with
  | BoxJoin (_, _) ->
      assert false (* simplified *)
  | Compr ([], _) ->
      assert false (* impossible *)
  | None_ ->
      E.none
  | Univ ->
      E.univ
  | Iden ->
      E.iden
  | Ident (Var v) ->
      E.var ~ar @@ get_var v stack
  | Ident (Name n) ->
      E.name ~ar n
  | RUn (op, e) ->
      E.runary ~ar (convert_runop op) (convert_exp stack e)
  | RBin (e1, op, e2) ->
      E.rbinary
        ~ar
        (convert_exp stack e1)
        (convert_rbinop op)
        (convert_exp stack e2)
  | RIte (c, t, e) ->
      E.rite
        ~ar
        (convert_fml stack c)
        (convert_exp stack t)
        (convert_exp stack e)
  | Prime e ->
      E.prime ~ar @@ convert_exp stack e
  | Compr (decls, block) ->
      let decls' = convert_sim_bindings stack decls in
      let vars = List.flat_map (fun (_, vars, _) -> vars) decls in
      let block' = convert_block (new_env vars stack) block in
      E.compr ~ar decls' block'
      |> Fun.tap (fun e ->
             Msg.debug (fun m ->
                 m
                   "Ast_to_elo.convert_Compr@ @[<hov2>%a@]@ --> @[<hov2>%a@]"
                   Ast.pp_prim_exp
                   prim_exp
                   (E.pp_exp 0)
                   e))


and convert_sim_bindings
    stack (decls : (Ast.var, Ast.ident) Gen_goal.sim_binding list) =
  match decls with
  | [] ->
      []
  | (disj, vars, range) :: tl ->
      let hd' = (disj, List.length vars, convert_exp stack range) in
      hd' :: convert_sim_bindings (new_env vars stack) tl


and convert_runop (op : Gen_goal.runop) =
  match op with
  | Transpose ->
      E.transpose
  | TClos ->
      E.tclos
  | RTClos ->
      E.rtclos


and convert_rbinop (op : Gen_goal.rbinop) =
  match op with
  | Union ->
      E.union
  | Inter ->
      E.inter
  | Over ->
      E.over
  | LProj ->
      E.lproj
  | RProj ->
      E.rproj
  | Prod ->
      E.prod
  | Diff ->
      E.diff
  | Join ->
      E.join


and convert_iexp stack ({ prim_iexp; _ } : (Ast.var, Ast.ident) Gen_goal.iexp) =
  match prim_iexp with
  | Num n ->
      E.num n
  | Card e ->
      E.card @@ convert_exp stack e
  | IUn (Neg, e) ->
      E.iunary E.neg @@ convert_iexp stack e
  | IBin (e1, op, e2) ->
      E.ibinary
        (convert_iexp stack e1)
        (convert_ibinop op)
        (convert_iexp stack e2)


and convert_ibinop (op : Gen_goal.ibinop) =
  match op with Add -> E.add | Sub -> E.sub


let convert_goal (Gen_goal.Run (fmls, expec)) =
  E.run (convert_block [] fmls) expec


let convert (ast : Ast.t) =
  let invariants = convert_block [] ast.invariants in
  let goal = convert_goal ast.goal in
  E.make
    ast.file
    ast.domain
    ast.instance
    ast.sym
    invariants
    goal
    ast.atom_renaming
    ast.name_renaming
