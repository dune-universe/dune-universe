(* Copyright (C) Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)

open Support
open Term

type parent_descriptor = Class_table.parent_descriptor

val inherit_parents: int -> Tvars.t -> inherit_clause -> Proof_context.t -> unit

val add_new_feature: info -> int -> Proof_context.t -> unit
