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


type t = Relation.t Map.t

let empty =
  Map.empty


let mem = Map.mem

let add name rel domain =
  assert (not @@ Map.mem name domain);
  Map.add name rel domain

let get_exn = Map.find

let get = Map.get

let to_list = Map.to_list

let univ_atoms domain =
  let open Relation in
  let open Scope in
  match get_exn Name.univ domain with
    | Const { scope; _ } ->
        (match scope with
          | Exact b -> b
          | Inexact _ -> assert false)
    | Var _ -> assert false
    | exception Not_found -> assert false

let pp out rels =
  Fmtc.(vbox @@
        Map.pp ~sep:" " ~arrow:" : " ~start:"" ~stop:""
          (styled `Cyan Name.pp) (Relation.pp ~print_name:false))
    out rels

let must name domain =
  assert (mem name domain);
  get_exn name domain
  |> Relation.scope
  |> Scope.must

let may name domain =
  assert (mem name domain);
  get_exn name domain
  |> Relation.scope
  |> Scope.may

let sup name domain =
  assert (mem name domain);
  get_exn name domain
  |> Relation.scope
  |> Scope.sup


let musts ?(with_univ_and_ident = true) domain =
  (if with_univ_and_ident then domain
   else (domain |> Map.remove Name.univ |> Map.remove Name.iden))
  |>  Map.map Relation.must 
  |> to_list

let arities =
  Fun.(Map.map Relation.arity %> to_list)

let update_domain_with_instance domain instance =
  let module R = Relation in
  let module I = Instance in
  let relation_of_instance_item inst_item rel =
    assert (R.is_const rel);
    R.const (R.name rel) (R.arity rel) (Scope.exact I.(inst_item))
  in
  let keep_instance __name = function
    | `Both (dom_entry, inst_entry) ->
        Some (relation_of_instance_item inst_entry dom_entry)
    | `Left dom_entry -> Some dom_entry
    | `Right _ ->
        (* cannot happen: Raw_to_elo checked that every 
           instance is in the domain *)
        assert false
  in
  Map.merge_safe ~f:keep_instance domain (Instance.to_map instance)
  


module P = Intf.Print.Mixin(struct type nonrec t = t let pp = pp end)
include P 
 
