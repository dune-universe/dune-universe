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

(** Identifiers in "raw" ASTs. *)

(** Any form of identifier for constants (atoms, relations) in the source
    code. *)
type t = {
  ident : string;
  loc : Location.t
}

(** {1 Constructor}  *)

val ident : string -> Lexing.position -> Lexing.position -> t

(** {1 Accessors} *)
  
val basename : t -> string

val location : t -> Location.t

val eq_name : t -> t -> bool


