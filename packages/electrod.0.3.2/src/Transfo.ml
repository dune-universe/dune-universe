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

(** Type of transformations. *)

open Containers

type ('src, 'dst) t =
  { name : string
  ; run : 'src -> 'dst
  }

let make name run =
  assert (not @@ String.is_empty name) ;
  { name; run }


let name { name; _ } = name

let run t x = t.run x

let fby t1 t2 =
  { name = t1.name ^ "$$" ^ t2.name; run = (fun x -> t1.run x |> t2.run) }


let identity = { name = "$$id"; run = (fun x -> x) }

(* association list *)
type ('src, 'dst) tlist = (string * ('src, 'dst) t) list

let tlist ts =
  assert (not @@ List.is_empty ts) ;
  let open List in
  let add transfos t =
    assert (not @@ Assoc.mem ~eq:String.equal t.name transfos) ;
    Assoc.set ~eq:String.equal t.name t transfos
  in
  fold_left add [] ts


let get_exn ts t = List.Assoc.get_exn ~eq:String.equal t ts
