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

(** Compared to Simplify1, this version maps qualified relations to formulas
    relying on cardinality arguments. *)

(** EXPECTED TO BE BE DONE AFTER CHECKING ARITIES. *)

open Containers
open Gen_goal
module TS = Tuple_set

let fresh_var base exp = Var.fresh ~loc:exp.exp_loc base

(* simplify Ast goals *)
class simplify =
  object (self : 'self)
    inherit Simplify1.simplify

    (* change relation qualifiers into formulas *)
    method! visit_Qual env qual exp =
      Msg.debug (fun m ->
          m "Simplify2.visit_Qual <-- %a" Ast.pp_prim_fml
          @@ Gen_goal.qual qual exp) ;
      let prim_fml =
        match qual with
        | ROne ->
            icomp (iexp exp.exp_loc @@ card exp) ieq (iexp exp.exp_loc @@ num 1)
        | RLone ->
            icomp (iexp exp.exp_loc @@ card exp) lte (iexp exp.exp_loc @@ num 1)
        | RSome ->
            icomp (iexp exp.exp_loc @@ card exp) gte (iexp exp.exp_loc @@ num 1)
        | RNo ->
            icomp (iexp exp.exp_loc @@ card exp) ieq (iexp exp.exp_loc @@ num 0)
      in
      self#visit_prim_fml env prim_fml
      |> Fun.tap
         @@ fun res ->
         Msg.debug (fun m ->
             m "Simplify2.visit_Qual --> %a" Ast.pp_prim_fml res)
  end

let run elo =
  let open Ast in
  Msg.debug (fun m -> m "Entering Simplify2.simplify_fml") ;
  { elo with goal = (new simplify)#visit_t () elo.goal }


let transfo = Transfo.make "simplify2" run
