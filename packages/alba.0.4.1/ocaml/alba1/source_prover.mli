(* Copyright (C) Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)

open Support

module PC = Proof_context

val prove_and_store:
    entities list withinfo
  -> compound -> compound -> source_proof -> PC.t
    -> unit
