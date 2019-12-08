(*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2019 ONERA
 * Authors: Julien Brunel (ONERA), David Chemouil (ONERA)
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * 
 * SPDX-License-Identifier: MPL-2.0
 * License-Filename: LICENSE.md
 ******************************************************************************)

(** Simplifies Electrod models. *)

(** EXPECTED TO BE BE DONE AFTER CHECKING ARITIES. *)

open Containers
open Gen_goal
module TS = Tuple_set
module L = Location

let fresh_var base exp = Var.fresh ~loc:exp.exp_loc base

(* from the var list [x1, x2, ...] 
   create [x1', x2', ...] and the assoc list [(x1, x1'), (x2, x2') ...] 
   and the formula x1!=x1' or x2!=x2' or ...  *)
let create_new_vars_and_assoc_list_and_comp_fml vs ar =
  List.fold_right
    (fun (Ast.BVar var) (new_vars, assoc, prim_fml) ->
      let new_var = Var.fresh_copy var in
      let new_var_as_ident = ident (Ast.var_ident new_var) in
      ( Ast.bound_var new_var :: new_vars
      , CCList.Assoc.set ~eq:Var.equal var new_var_as_ident assoc
      , lbinary
          ( fml L.dummy
          @@ rcomp
               (exp ar L.dummy @@ ident @@ Ast.var_ident var)
               req
               (exp ar L.dummy new_var_as_ident) )
          and_
          (fml L.dummy prim_fml) ))
    vs
    ([], [], true_)


(* simplify Ast goals *)
class simplify =
  object (self : 'self)
    inherit [_] map as super

    (* the environment is not used, setting it here at [()] to avoid unbound type
     variables *)
    method visit_'v () = Fun.id

    method visit_'i _ = Fun.id

    method visit_Quant_One env __q sim_bindings blk =
      (*re-write one x1,y1:r1, x2,y2:r2 | phi into
      some x1,y1:r1, x2,y2:r2 | (phi and all x1',x2':r1, x2',y2';r2 | ...)*)
      let new_sim_bindings, assoc_new_vars, cmp_fml =
        List.fold_right
          (fun (disj, vars, e) (sim_bindings, acc_assoc, acc_fml) ->
            let new_vars, assoc, prim_fml =
              create_new_vars_and_assoc_list_and_comp_fml vars e.arity
            in
            ( (disj, new_vars, e) :: sim_bindings
            , assoc @ acc_assoc
            , lbinary (fml L.dummy prim_fml) and_ (fml L.dummy acc_fml) ))
          sim_bindings
          ([], [], true_)
      in
      (* subst_blk = phi [x1'\x1, x2'\x2, ...] *)
      let subst_blk = Ast.substitute#visit_block assoc_new_vars blk in
      (* conversion of the block in a formula *)
      let fml_subst_blk =
        List.fold_right
          (fun curfml acc_fml -> fml L.dummy @@ lbinary curfml and_ acc_fml)
          subst_blk
          (fml L.dummy true_)
      in
      let temp_forall_fml =
        quant
          all
          new_sim_bindings
          [ fml L.dummy @@ lbinary fml_subst_blk impl (fml L.dummy cmp_fml) ]
      in
      let temporary_fml =
        quant
          some
          sim_bindings
          [ fml L.dummy
            @@ lbinary
                 (fml L.dummy (block blk))
                 and_
                 (fml L.dummy temp_forall_fml)
          ]
      in
      self#visit_prim_fml env temporary_fml

    (* translate lone x:t|phi into one x:t|phi or no x:t|phi *)
    method visit_Quant_Lone env __q sim_bindings blk =
      let res_fml =
        lbinary
          (fml L.dummy @@ quant one sim_bindings blk)
          or_
          (fml L.dummy @@ quant no_ sim_bindings blk)
      in
      self#visit_prim_fml env res_fml

    (* split multiple simultaneous All/Some/No bindings into many quantifications *)
    method! visit_Quant env q sim_bindings blk =
      Msg.debug (fun m ->
          m "Simplify1.visit_Quant <-- %a" Ast.pp_prim_fml
          @@ quant q sim_bindings blk);
      match q with
      | One ->
          self#visit_Quant_One env q sim_bindings blk
      | Lone ->
          self#visit_Quant_Lone env q sim_bindings blk
      | All | Some_ | No ->
          let res =
            match sim_bindings with
            | [] ->
                assert false
            | [ (disj, vs, e) ] ->
                let blk' = List.map (self#visit_fml env) blk in
                quant q [ (disj, vs, self#visit_exp env e) ] blk'
            | (disj, vs, e) :: bs ->
                quant
                  q
                  [ (disj, vs, self#visit_exp env e) ]
                  [ fml e.exp_loc @@ self#visit_Quant env q bs blk ]
          in
          Msg.debug (fun m ->
              m "Simplify1.visit_Quant --> %a" Ast.pp_prim_fml res);
          res

    (* substitute let bindings *)
    method! visit_Let env bindings fmls =
      (* substitute from right to left as a binding on the left may apply in the
       range of a binding on the right *)
      (* Msg.debug (fun m -> m "Simplify1.visit_Let <-- %a" *)
      (*                       Ast.pp_prim_fml *)
      (*             @@ let_ bindings fmls); *)
      List.fold_right
        (function
          | Ast.BVar v, e -> Ast.substitute#visit_prim_fml [ (v, e.prim_exp) ])
        bindings
        (block fmls)
      |> self#visit_prim_fml env

    (* |> Fun.tap *)
    (* @@ fun res -> *)
    (* Msg.debug (fun m -> m "Simplify1.visit_Let --> %a" *)
    (*                       Ast.pp_prim_fml res) *)

    (* change relation qualifiers into formulas *)
    method! visit_Qual env q expr =
      (* Msg.debug (fun m -> m "Simplify1.visit_Qual <-- %a" *)
      (*                       Ast.pp_prim_fml *)
      (*             @@ qual q expr); *)
      let prim_fml =
        match q with
        | ROne ->
            quant
              one
              [ (false, [ Ast.bound_var @@ fresh_var "one" expr ], expr) ]
              [ fml expr.exp_loc true_ ]
        | RLone ->
            lbinary
              (fml expr.exp_loc @@ qual rno expr)
              or_
              (fml expr.exp_loc @@ qual rone expr)
        | RSome ->
            quant
              some
              [ (false, [ Ast.bound_var @@ fresh_var "some" expr ], expr) ]
              [ fml expr.exp_loc true_ ]
        | RNo ->
            rcomp expr in_ @@ exp None expr.exp_loc none
      in
      self#visit_prim_fml env prim_fml

    (* |> Fun.tap *)
    (* @@ fun res -> *)
    (* Msg.debug (fun m -> m "Simplify1.visit_Qual --> %a" *)
    (*                       Ast.pp_prim_fml res) *)

    (* change box join in join *)
    method! visit_BoxJoin env call args =
      (* Msg.debug (fun m -> m "Simplify1.visit_BoxJoin <-- %a[%a]" *)
      (*                       Ast.pp_exp call *)
      (*                       (Fmtc.list Ast.pp_exp) args); *)
      let res =
        List.fold_right
          (fun arg r ->
            exp
              Option.(return @@ (get_exn arg.arity + get_exn r.arity - 2))
              L.(span (arg.exp_loc, r.exp_loc))
            @@ rbinary arg join r)
          args
          call
      in
      self#visit_prim_exp env res.prim_exp

    (* |> Fun.tap *)
    (* @@ fun res -> *)
    (* Msg.debug (fun m -> m "Simplify1.visit_BoxJoin --> %a" *)
    (*                       Ast.pp_prim_exp *)
    (*                       res) *)

    (* reintroducing lost implications*)
    method! visit_prim_fml env f =
      match f with
      | LBin ({ prim_fml = LUn (Not, f1); _ }, Or, f2) ->
          let f1' = super#visit_fml env f1
          and f2' = super#visit_fml env f2 in
          lbinary f1' impl f2'
      | True
      | False
      | Qual (_, _)
      | LBin (_, _, _)
      | RComp (_, _, _)
      | IComp (_, _, _)
      | LUn (_, _)
      | Quant (_, _, _)
      | Let (_, _)
      | FIte (_, _, _)
      | Block _ ->
          super#visit_prim_fml env f
  end

let run elo =
  let open Ast in
  (* Msg.debug (fun m -> m "Entering Simplify1.simplify_fml"); *)
  let simpl = new simplify in
  { elo with
    goal = simpl#visit_t () elo.goal
  ; invariants = simpl#visit_block () elo.invariants
  }


(* |> Fun.tap (fun _ -> Msg.debug (fun m -> m "Finished Simplify1.simplify_fml")) *)

let transfo = Transfo.make "simplify1" run
