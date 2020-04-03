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

(** Type for "raw" ASTs yielded by the Electrod parser. *)

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

and raw_urelements = private
  | UIntvl of raw_interval
  | UPlain of Raw_ident.t

(** The [int option] is the optionally-declared arity (compulsory for an empty
    scope). *)
and raw_declaration = private
  | DConst of Raw_ident.t * int option * raw_scope
  | DVar of Raw_ident.t * int option * raw_scope * raw_scope option

and raw_multiplicity = [ `Lone | `One ] option

and raw_scope = private
  | SExact of raw_bound
  | SInexact of raw_bound * raw_multiplicity * raw_bound

and raw_bound = private
  | BUniv
  | BRef of Raw_ident.t  (** reference to a previously-defined {i set} *)
  | BProd of raw_bound * raw_multiplicity * raw_bound
      (** None/Some (lone/one) *)
  | BUnion of raw_bound * raw_bound
  | BElts of raw_element list

and raw_element = private
  | EIntvl of raw_interval  (** 1-tuples *)
  | ETuple of raw_tuple

and raw_tuple = Raw_ident.t list
(** A n-tuple (incl. n = 1). inv: nonempty list *)

and raw_interval = Raw_ident.t * Raw_ident.t

(** asignemnt of tuples to a relation *)
and raw_assignment = Raw_ident.t * raw_tuple list
(** may be empty  *)

and raw_symmetry =
  (Raw_ident.t * raw_tuple) list * (Raw_ident.t * raw_tuple) list

(** This definition is here just to be used in the parser ((to avoid cyclic
    dependencies). The puprose of paragraphs is to deal easily and efficiently
    with {i permutation} of these (see {!Parse_main} for more
    information).)  *)
type raw_paragraph =
  | ParGoal of raw_goal
  | ParInst of raw_assignment list
  | ParSym of raw_symmetry list
  | ParInv of raw_block

(** {1 Constructors} *)

val interval : Raw_ident.t -> Raw_ident.t -> raw_interval

val etuple : Raw_ident.t list -> raw_element

val eintvl : raw_interval -> raw_element

val buniv : raw_bound

val bref : Raw_ident.t -> raw_bound

val bprod : raw_bound -> raw_multiplicity -> raw_bound -> raw_bound

val bunion : raw_bound -> raw_bound -> raw_bound

val belts : raw_element list -> raw_bound

val sexact : raw_bound -> raw_scope

val sinexact : raw_bound -> raw_multiplicity -> raw_bound -> raw_scope

val dconst : Raw_ident.t -> int option -> raw_scope -> raw_declaration

val dvar :
  Raw_ident.t -> int option -> raw_scope -> raw_scope option -> raw_declaration

val uintvl : raw_interval -> raw_urelements

val uplain : Raw_ident.t -> raw_urelements

val problem :
     string option
  -> raw_urelements list
  -> raw_declaration list
  -> raw_goal
  -> raw_block
  -> raw_assignment list
  -> raw_symmetry list
  -> raw_problem

(** {1 Accessors} *)

val decl_id : raw_declaration -> Raw_ident.t
