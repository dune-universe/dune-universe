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

(** Type of relations. *)

(** A relation is either static (const) or dynamic (var). In the latter case, it
    may either be specified in a unique scope or with a scope for the initial
    instant and then a scope for all other instants. The arity is compulsorily
    specified by the user for empty bounds and optionally otherwise.*)
type t =
  | Const of {name : Name.t; arity : int; scope : Scope.t}
  | Var of {name : Name.t; arity : int; scope : Scope.t; fby : Scope.t option}

val const : Name.t -> int -> Scope.t -> t
(** {1 Constructors} *)

val var : Name.t -> int -> Scope.t -> Scope.t option -> t

val arity : t -> int
(** Arity of the relation. (> 0) *)

val name : t -> Name.t

val equal : t -> t -> bool

val is_set : t -> bool
(** Tells whether the relation is a set or a relation of arity > 1. *)

val is_nary : t -> bool

val is_const : t -> bool

val is_var : t -> bool

val scope : t -> Scope.t
(** Returns the scope of a relation (for variable relations: not [fby]!)  *)

val must : t -> Tuple_set.t

val may : t -> Tuple_set.t

val sup : t -> Tuple_set.t

val rename :
  (Atom.t, Atom.t) List.Assoc.t -> (Name.t, Name.t) List.Assoc.t -> t -> t

val pp : ?print_name:bool -> Format.formatter -> t -> unit

val to_string : ?print_name:bool -> t -> string
