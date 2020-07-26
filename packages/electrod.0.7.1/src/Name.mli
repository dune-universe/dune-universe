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

(** Names (of relations for instance). *)

type t

val name : string -> t
(** {1 Constructors} *)

val dummy : unit -> t
(** Returns a dummy name. This function is only here for some computations that
    raise new relations and hence need a temporary, dummy name. *)

val of_raw_ident : Raw_ident.t -> t

val univ : t
(** Reserved name for 'univ' and 'iden'. *)

val iden : t

val equal : t -> t -> bool
(** Tells whether two names are the same *)

val compare : t -> t -> int

val hash : t -> int

val style : Fmt.style

include Intf.Print.S with type t := t

module Map : CCMap.S with type key = t
