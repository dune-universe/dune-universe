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

open Containers

(* Converts a natural number into a list of base-26 digits, in reversed order.
   E.g. [convert 27 = [0;1]] *)
let convert n =
  assert (n >= 0);
  let rec aux n =
    let q = n / 26
    and r = n mod 26 in
    if q = 0 then [ r ] else r :: aux q
  in
  aux n


(* Creates an encoding function of numbers into base 26, starting at character
   [base]. *)
let make_encode base =
  let base_code = CCChar.code base in
  fun n ->
    let list = convert n in
    List.rev_map (fun i -> CCChar.of_int_exn (i + base_code)) list
    |> CCString.of_list


(* lowercase letters for atoms *)
let encode_atom = make_encode 'a'

(* uppercase letters for relations *)
let encode_relation = make_encode 'A'

let is_univ_or_iden n = List.mem ~eq:Name.equal n [ Name.iden; Name.univ ]

let compute_relation_renaming elo =
  Domain.to_list elo.Ast.domain
  |> List.mapi (fun i (name, _) ->
         if is_univ_or_iden name
         then (name, name) (* do not rename univ and iden *)
         else
           let new_string = encode_relation i in
           let new_name = Name.name new_string in
           if is_univ_or_iden new_name
           then
             (* it may happen that new_string happens to fall on "univ" or
               "iden", which may induce errors in the translation process, so we
               append a symbol (1) not used in the encoding set *)
             (name, Name.name @@ new_string ^ "1")
           else (name, new_name))


let compute_atom_renaming elo =
  Domain.univ_atoms elo.Ast.domain
  |> Tuple_set.to_list
  |> List.mapi (fun i tuple ->
         let atom = Tuple.ith 0 tuple in
         let new_atom = Atom.atom @@ encode_atom i in
         (atom, new_atom))


let rename_elo long_names elo =
  if long_names
  then
    (* long_names = true ==> renaming is identity *)
    let id_renaming l = List.map (fun x -> (x, x)) l in
    Ast.
      { elo with
        atom_renaming =
          id_renaming
            ( Domain.univ_atoms elo.Ast.domain
            |> Tuple_set.to_list
            |> List.map (Tuple.ith 0) )
      ; name_renaming =
          id_renaming (Domain.to_list elo.Ast.domain |> List.map fst)
      }
  else
    let atom_renaming = compute_atom_renaming elo in
    let name_renaming = compute_relation_renaming elo in
    let open Fmtc in
    Msg.debug (fun m ->
        m
          "Atom renaming:@ %a"
          ( brackets
          @@ list ~sep:semi
          @@ parens
          @@ pair ~sep:comma Atom.pp Atom.pp )
          atom_renaming);
    Msg.debug (fun m ->
        m
          "Name renaming:@ %a"
          ( brackets
          @@ list ~sep:semi
          @@ parens
          @@ pair ~sep:comma Name.pp Name.pp )
          name_renaming);
    Ast.
      { elo with
        domain = Domain.rename atom_renaming name_renaming elo.domain
      ; goal = Ast.rename#visit_t name_renaming elo.goal
      ; invariants =
          List.map (Ast.rename#visit_fml name_renaming) elo.invariants
      ; sym = List.map (Symmetry.rename atom_renaming name_renaming) elo.sym
      ; instance = Instance.rename atom_renaming name_renaming elo.instance
      ; atom_renaming
      ; name_renaming
      }
