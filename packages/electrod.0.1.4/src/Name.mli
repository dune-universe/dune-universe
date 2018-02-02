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

(** Names (of relations for instance). *)

type t 

(** {1 Constructors} *)
val name : string -> t

(** Returns a dummy name. This function is only here for some computations that
    raise new relations and hence need a temporary, dummy name. *)
val dummy : unit -> t

val of_raw_ident : Raw_ident.t -> t

(** Reserved name for 'univ' and 'iden'. *)
val univ : t
val iden : t

(** Tells whether two names are the same *)
val equal : t -> t -> bool
  
val compare : t -> t -> int

val style : Fmt.style

include Intf.Print.S with type t := t


module Map : CCMap.S with type key = t
