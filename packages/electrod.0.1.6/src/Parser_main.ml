(*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2018 ONERA
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

module P = Parser

(* To allow paragraphs to appear in any order, or even, for some, not to appear,
   the grammar says that a list of paragraphs is allowed. Then this list has to be
   checked to see whether it corresponds to a legit permutation (defining legit
   permutations inside the grammar would be cumbersome and less efficient) *)
let check_paragraphs file pars =
  let open List in
  let goal =
    let candidates =
      filter
        (function Raw.ParGoal _ -> true
                | Raw.ParInst _ | Raw.ParSym _ | Raw.ParInv _ -> false) pars in
    if length candidates = 1 then (* there must be one goal *)
      match candidates with [ Raw.ParGoal g ] -> g | _ -> assert false
    else
      Msg.Fatal.syntax_error_paragraphs
        (fun args -> args file "one goal must be declared exactly")
  in
  let invar =
    let candidates =
      filter (function Raw.ParInv _ -> true
                     | Raw.ParGoal _ | Raw.ParInst _ | Raw.ParSym _ -> false) pars in
    if length candidates <= 1 then (* there may be one list of instances *)
      match candidates with [] -> [] | [ Raw.ParInv g ] -> g | _ -> assert false
    else 
      Msg.Fatal.syntax_error_paragraphs
        (fun args -> args file "at most one invariant section may be declared")
  in
  let inst =
    let candidates =
      filter (function Raw.ParInst _ -> true
                     | Raw.ParGoal _ | Raw.ParSym _ | Raw.ParInv _ -> false) pars in
    if length candidates <= 1 then (* there may be one list of instances *)
      match candidates with [] -> [] | [ Raw.ParInst g ] -> g | _ -> assert false
    else 
      Msg.Fatal.syntax_error_paragraphs
        (fun args -> args file "at most one (partial) instance may be declared")
  in
  let sym =
    let candidates =
      filter (function Raw.ParSym _ -> true
                     | Raw.ParGoal _ | Raw.ParInst _ | Raw.ParInv _ -> false) pars in
    if length candidates <= 1 then  (* there may be one list of symmetries *)
      match candidates with [] -> [] | [ Raw.ParSym g ] -> g | _ -> assert false
    else
      Msg.Fatal.syntax_error_paragraphs
        (fun args -> args file "at most one list of symmetries may be declared")
  in
  (goal, invar, inst, sym)


let parse_file file = 
  IO.with_in file @@
  fun ic ->
  let lexbuf = Lexing.from_channel ic in
  try 
    let raw_univ, raw_decls, raw_paragraphs =
      P.parse_problem (Scanner.main (Some file)) lexbuf
    in
    let (raw_goal, raw_fact, raw_inst, raw_syms) =
      check_paragraphs (Some file) raw_paragraphs in
    Raw.problem (Some file) raw_univ raw_decls raw_goal raw_fact raw_inst raw_syms
  with P.Error ->
    Msg.Fatal.syntax @@ fun args -> args file lexbuf

let parse_string s = 
  let lexbuf = Lexing.from_string s in
  let raw_univ, raw_decls, raw_paragraphs =
    P.parse_problem (Scanner.main None) lexbuf in
  let (raw_goal, raw_fact, raw_inst, raw_syms) =
    check_paragraphs None raw_paragraphs in
  Raw.problem None raw_univ raw_decls raw_goal raw_fact raw_inst raw_syms


