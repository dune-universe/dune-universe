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
open Lexing

type t =
  { begp : Lexing.position
  ; endp : Lexing.position }

let from_positions begp endp =
  assert (begp.pos_cnum <= endp.pos_cnum);
  {begp; endp}


let begl {begp; _} = begp.pos_lnum

let begc {begp; _} = begp.pos_cnum - begp.pos_bol

let endl {endp; _} = endp.pos_lnum

let endc {endp; _} = endp.pos_cnum - endp.pos_bol

let to_ints loc = ((begl loc, begc loc), (endl loc, endc loc))

let span (loc1, loc2) =
  let begp =
    if loc1.begp.pos_cnum < loc2.begp.pos_cnum then loc1.begp else loc2.begp
  in
  let endp =
    if loc1.endp.pos_cnum > loc2.endp.pos_cnum then loc1.endp else loc2.endp
  in
  from_positions begp endp


let dummy = {begp = Lexing.dummy_pos; endp = Lexing.dummy_pos}

type 'a located =
  { data : 'a
  ; loc : t }

let make_located data loc = {data; loc}

let pp_located pp out {data; _} = pp out data

let pp out loc = Fmtc.text_loc out @@ to_ints loc

module P = Intf.Print.Mixin (struct
  type nonrec t = t

  let pp = pp
end)

include P
