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

(** Defines traits and mixins to use about anywhere. *)

module Print = struct
  module type S = sig
    type t

    val pp : t Fmtc.t

    val to_string : t -> string
  end

  module Mixin (M : sig
    type t

    val pp : t Fmtc.t
  end) : S with type t := M.t = struct
    include M

    let to_string = Fmtc.to_to_string pp
  end
end

module type COMPARE = sig
  type t

  val compare : t -> t -> int

  val equal : t -> t -> bool
end
