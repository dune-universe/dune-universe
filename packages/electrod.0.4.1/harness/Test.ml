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

module Q = QCheck
module G = Q.Gen

(* GENERATORS *)

let atom_chars = G.oneofl [ "a"; "b"; "c"; "d"; "e"; "f"; "g" ]

let gen_atom = G.(map (Atom.atom ~loc:Location.dummy) atom_chars)

let gen_tuple ?(first = 1) ?(last = 5) =
  G.(map Tuple.of_list1 @@ list_size (first -- last) gen_atom)


let gen_tuple2 = gen_tuple ~first:2 ~last:2

let gen_tupleset ?arity =
  G.(
    map TupleSet.of_tuples
    @@ list_size (0 -- 20)
    @@
    match arity with
    | None ->
        let ar = generate1 (1 -- 5) in
        gen_tuple ~first:ar ~last:ar
    | Some ar ->
        gen_tuple ~first:ar ~last:ar)


(* "ARBITRARIES" (in QCheck parlance) *)

let any_tuple2 = Q.make ~print:Tuple.to_string gen_tuple2

let any_tuple = Q.make ~print:Tuple.to_string gen_tuple

let any_tupleset = Q.make ~print:TupleSet.to_string gen_tupleset

let any_tupleset1 = Q.make ~print:TupleSet.to_string @@ gen_tupleset ~arity:1

let any_tupleset2 = Q.make ~print:TupleSet.to_string @@ gen_tupleset ~arity:2
