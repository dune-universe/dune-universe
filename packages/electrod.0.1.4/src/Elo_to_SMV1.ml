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

(** Provides a transformation from Electrod models to SMV models. Uses
   enumerations when possible. *)

open Containers

  
module SMV_atom : Solver.ATOMIC_PROPOSITION = struct
  type t = {
    sym : Symbol.t;             (* hashconsed strings *)
    const : bool;
    partial : bool;         (* is 'lone'? *)
    dom_arity : int option;     (* arity of the domain (>=0) if functional, else None *)
  }
  
  let compare { sym = sym1; _}  { sym = sym2; _ } = Symbol.compare sym1 sym2
                                                      
  let compare_string { sym = sym1; _}  { sym = sym2; _ } =
    Symbol.compare_string sym1 sym2

  let pp fmt at = Symbol.pp fmt at.sym

  let equal { sym = sym1; _}  { sym = sym2; _ } = Symbol.equal sym1 sym2

  let hash at = Symbol.hash at.sym

  let domain_arity t = t.dom_arity

  let is_const t = t.const
                     
  let is_partial t = t.partial

  (* table tracking which pair (name, tuple) a string comes from. Uses
     hahsconsing to make this more efficient *)
  module HT = CCHashtbl.Make(Symbol)
      
  let names_and_tuples = HT.create 179 (* usually less than that many VARs *)
             
  let rel_sep = "-"

  let atom_sep = Fmtc.minus
  
  let make domain name tuple =
    let rel = Domain.get_exn name domain in
    let dom_arity =
      let open Scope in
      match Relation.scope rel with
        | Exact _ -> assert false
        | Inexact Plain_relation _ -> None
        | Inexact Partial_function (ar, _) -> Some ar
        | Inexact Total_function (ar, _) -> Some ar
    in
    let const = Relation.is_const rel in
    let partial = rel |> Relation.scope |> Scope.is_partial in
    let ats = Tuple.to_list tuple in
    let name_str =
      let s = Fmtc.to_to_string Name.pp name in
      if String.prefix ~pre:"$" s then
        (* Skolem vars may have a name incompatible with SMV so: *)
        "_" ^ s
      else s
    in
    let full_str =
      Format.sprintf "%s%s%a"
        name_str
        rel_sep
        Fmtc.(list ~sep:atom_sep Atom.pp) ats
    in 
    let sym = Symbol.make full_str in
    (* keep track of creations to allow to get original pairs back *)
    HT.add names_and_tuples sym (name, tuple);
    { sym; dom_arity; const; partial }
  
  let split at =
    HT.get names_and_tuples at.sym
                                 
  let split_string str =
    HT.get names_and_tuples (Symbol.make str)
   
end

module SMV_LTL = SMV.Make_SMV_LTL(SMV_atom)

module SMV_file_format = SMV.Make_SMV_file_format(SMV_LTL)

module Elo_to_SMV_LTL = Elo_to_LTL1.Make(SMV_LTL)

module Elo_to_SMV_model = Elo_to_model1.Make(SMV_LTL)(Elo_to_SMV_LTL)(SMV_file_format)

let pp = SMV_file_format.pp

let analyze = SMV_file_format.analyze
           
(* temporary *)
let run elo =
  Elo_to_SMV_model.run elo

let transfo = Transfo.make "to_smv1" run
