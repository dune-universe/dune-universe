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

open Containers

type t = (Name.t * Tuple.t) list * (Name.t * Tuple.t) list

let make x y = (x, y)

let fold f (sym : t) acc =
  let l1, l2 = sym in
  List.fold_right2 f l1 l2 acc


let rename atom_renaming relation_renaming (left, right) =
  let rename_list l =
    List.map
      (fun (name, tuple) ->
        ( List.assoc ~eq:Name.equal name relation_renaming
        , Tuple.rename atom_renaming tuple ))
      l
  in
  (rename_list left, rename_list right)
