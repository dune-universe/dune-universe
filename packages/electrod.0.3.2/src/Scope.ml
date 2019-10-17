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
module TS = Tuple_set

type relation =
  | Plain_relation of TS.t * TS.t
  | Partial_function of int * TS.t
  | Total_function of int * TS.t

type t =
  | Exact of TS.t
  | Inexact of relation

let equal sc1 sc2 =
  match (sc1, sc2) with
  | Exact ts1, Exact ts2 ->
      TS.equal ts1 ts2
  | Exact _, Inexact _ | Inexact _, Exact _ ->
      false
  | Inexact r1, Inexact r2 ->
    ( match (r1, r2) with
    | Plain_relation (r11, r12), Plain_relation (r21, r22) ->
        TS.equal r11 r21 && TS.equal r12 r22
    | Partial_function (dom_ar1, sup1), Partial_function (dom_ar2, sup2)
    | Total_function (dom_ar1, sup1), Total_function (dom_ar2, sup2) ->
        dom_ar1 = dom_ar2 && TS.equal sup1 sup2
    | Plain_relation _, (Partial_function _ | Total_function _)
    | Partial_function _, (Plain_relation _ | Total_function _)
    | Total_function _, (Plain_relation _ | Partial_function _) ->
        false )


let exact bound = Exact bound

let plain_relation inf sup =
  assert (TS.(inferred_arity inf = inferred_arity sup || TS.is_empty inf)) ;
  assert (TS.(size sup >= size inf)) ;
  Plain_relation (inf, sup)


let partial_function dom_arity sup =
  assert (dom_arity >= 0) ;
  assert (dom_arity < TS.inferred_arity sup) ;
  Partial_function (dom_arity, sup)


let total_function dom_arity sup =
  assert (dom_arity >= 0) ;
  assert (dom_arity < TS.inferred_arity sup) ;
  Total_function (dom_arity, sup)


let inexact rel = Inexact rel

let is_partial = function
  | Inexact (Partial_function _) ->
      true
  | Inexact (Total_function _ | Plain_relation _) | Exact _ ->
      false


let inferred_arity = function
  | Exact b
  | Inexact
      (Plain_relation (_, b) | Partial_function (_, b) | Total_function (_, b))
    ->
      TS.inferred_arity b


let included_in tupleset = function
  | Exact exact ->
      TS.subset tupleset exact
  | Inexact (Plain_relation (inf, sup)) ->
      TS.subset inf tupleset && TS.subset tupleset sup
  | Inexact (Partial_function (_, sup) | Total_function (_, sup)) ->
      TS.subset tupleset sup


let inf = function
  | Exact ts | Inexact (Plain_relation (ts, _)) ->
      ts
  | Inexact (Partial_function _ | Total_function _) ->
      TS.empty


let sup = function
  | Exact ts | Inexact (Plain_relation (_, ts)) ->
      ts
  | Inexact (Partial_function (_, sup) | Total_function (_, sup)) ->
      sup


let must = inf

let may_aux sc =
  assert (TS.subset (inf sc) (sup sc)) ;
  match sc with
  | Exact _ ->
      TS.empty
  | Inexact (Plain_relation (inf, sup)) ->
      TS.diff sup inf
  | Inexact (Partial_function (_, sup) | Total_function (_, sup)) ->
      sup


let may = CCCache.(with_cache (lru ~eq:equal 253) may_aux)

let pp out = function
  | Exact bound ->
      TS.pp out bound
  | Inexact (Plain_relation (inf, sup)) ->
      Fmtc.(box @@ pair ~sep:sp (box2 TS.pp) (box2 TS.pp)) out (inf, sup)
  | Inexact (Partial_function (dom_ar, sup)) ->
      Fmtc.(box @@ triple string int (box2 TS.pp)) out ("lone {}", dom_ar, sup)
  | Inexact (Total_function (dom_ar, sup)) ->
      Fmtc.(box @@ triple string int (box2 TS.pp)) out ("one {}", dom_ar, sup)


let rename atom_renaming = function
  | Exact bound ->
      Exact (TS.rename atom_renaming bound)
  | Inexact (Plain_relation (inf, sup)) ->
      Inexact
        (Plain_relation
           (TS.rename atom_renaming inf, TS.rename atom_renaming sup))
  | Inexact (Partial_function (dom_ar, sup)) ->
      Inexact (Partial_function (dom_ar, TS.rename atom_renaming sup))
  | Inexact (Total_function (dom_ar, sup)) ->
      Inexact (Total_function (dom_ar, TS.rename atom_renaming sup))


module P = Intf.Print.Mixin (struct
  type nonrec t = t

  let pp = pp
end)

include P
