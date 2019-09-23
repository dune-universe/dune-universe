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

(** Represents SMV files and how to produce them *)

val nuXmv_default_script : string

val nuSMV_default_script : string

val nuXmv_default_bmc_script : string

val nuSMV_default_bmc_script : string

module Make_SMV_LTL (At : Solver.ATOMIC_PROPOSITION) :
  Solver.LTL with module Atomic = At
(** Given an implementation for atoms, provides a LTL implementation with a
    pretty printing function for Solver formulas.  *)

(** TODO: implement abstract file format functions  *)
module Make_SMV_file_format (Ltl : Solver.LTL) :
  Solver.MODEL with type ltl = Ltl.t and type atomic = Ltl.Atomic.t
