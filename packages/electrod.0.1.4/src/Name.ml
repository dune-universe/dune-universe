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

open Containers
    
type t = string


let name s = s

let dummy =
  let c = ref 0 in
  fun () ->
    "dummy!" ^ (string_of_int @@ CCRef.get_then_incr c)

let of_raw_ident id = Raw_ident.basename id

let univ = "univ"

let iden = "iden"

let equal = String.equal

let compare = String.compare

let style = `Cyan

let pp out name =
  Fmtc.(styled style string) out name


module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
include P 

module Map = CCMap.Make(struct type t = string let compare = CCOrd.string end)
