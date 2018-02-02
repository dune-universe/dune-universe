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

module Map = Name.Map


type t = TupleSet.t Map.t

let empty = Map.empty

let mem = Map.mem

let add name rel ts =
  assert (not @@ Map.mem name ts);
  Map.add name rel ts

let get_exn = Map.find

let get = Map.get

let to_list = Map.to_list

let to_map x = x


let pp out rels =
  let open Fmtc in
  begin
    (styled `Bold pf) out "inst@ ";
    pf out "  %a"
      (vbox @@ Map.pp ~sep:" " ~arrow:" = " ~start:"" ~stop:""
                 (styled `Cyan Name.pp) TupleSet.pp) rels
  end


 


module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
include P 
 
