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

type raw_goal = (Raw_ident.t, Raw_ident.t) Gen_goal.t

type raw_block = (Raw_ident.t, Raw_ident.t) Gen_goal.block

type raw_problem =
  { file : string option
  ; raw_univ : raw_urelements list
  ; raw_decls : raw_declaration list  (** does not contain 'univ'  *)
  ; raw_goal : raw_goal
  ; raw_invar : raw_block  (** may be empty *)
  ; raw_inst : raw_assignment list  (** may be empty *)
  ; raw_syms : raw_symmetry list  (** may be empty *)
  }

and raw_urelements =
  | UIntvl of raw_interval
  | UPlain of Raw_ident.t

and raw_declaration =
  | DConst of Raw_ident.t * int option * raw_scope
  | DVar of Raw_ident.t * int option * raw_scope * raw_scope option

and raw_multiplicity = [ `Lone | `One ] option

and raw_scope =
  | SExact of raw_bound
  | SInexact of raw_bound * raw_multiplicity * raw_bound

and raw_bound =
  | BUniv
  | BRef of Raw_ident.t  (** reference to a previously-defined {i set} *)
  | BProd of raw_bound * raw_multiplicity * raw_bound
  | BUnion of raw_bound * raw_bound
  | BElts of raw_element list

and raw_element =
  | EIntvl of raw_interval  (** 1-tuples *)
  | ETuple of raw_tuple

(** A n-tuple (incl. n = 1). inv: nonempty list *)
and raw_tuple = Raw_ident.t list

and raw_interval = Raw_ident.t * Raw_ident.t

and raw_assignment = Raw_ident.t * raw_tuple list

and raw_symmetry =
  (Raw_ident.t * raw_tuple) list * (Raw_ident.t * raw_tuple) list

type raw_paragraph =
  | ParGoal of raw_goal
  | ParInst of raw_assignment list
  | ParSym of raw_symmetry list
  | ParInv of raw_block

let interval id1 id2 = (id1, id2)

let etuple ats =
  assert (not @@ List.is_empty ats);
  ETuple ats


let eintvl intvl = EIntvl intvl

let buniv = BUniv

let bref name = BRef name

let bprod mult b1 b2 = BProd (mult, b1, b2)

let bunion b1 b2 = BUnion (b1, b2)

let belts elts = BElts elts

let sexact b = SExact b

let sinexact b1 mult b2 = SInexact (b1, mult, b2)

let dconst atom arity scope = DConst (atom, arity, scope)

let dvar atom arity scope fby = DVar (atom, arity, scope, fby)

let uintvl intvl = UIntvl intvl

let uplain atom = UPlain atom

let problem file raw_univ raw_decls raw_goal raw_invar raw_inst raw_syms =
  { file; raw_univ; raw_decls; raw_goal; raw_invar; raw_inst; raw_syms }


let decl_id = function DConst (id, _, _) | DVar (id, _, _, _) -> id
