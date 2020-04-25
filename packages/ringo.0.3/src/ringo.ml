(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module type UNBOXED_COLLECTION = Sigs.UNBOXED_COLLECTION
module Ring : UNBOXED_COLLECTION = Ring
module Dll : UNBOXED_COLLECTION = Functors.Unbox(Dll)

type replacement =
  | LRU
  | FIFO

type overflow =
  | Strict
  | Weak

type accounting =
  | Precise
  | Sloppy

module type CACHE_MAP = Sigs.CACHE_MAP
module type CACHE_SET = Sigs.CACHE_SET

let tabler
: overflow -> (module Sigs.TABLER)
= function
  | Strict -> (module Functors.Strict_tabler)
  | Weak -> (module Functors.Weak_tabler)

let collection
: replacement -> accounting -> (module Sigs.COLLECTION)
= fun replacement accounting ->
  match replacement with
  | LRU -> (module Functors.LRU_Collection)
  | FIFO -> match accounting with
     | Precise -> (module Functors.FIFO_Precise_Collection)
     | Sloppy -> (module Functors.FIFO_Sloppy_Collection)

module type MAP_MAKER = functor (H: Hashtbl.HashedType) -> CACHE_MAP with type key = H.t
type map_maker = (module MAP_MAKER)

let map_maker ~replacement ~overflow ~accounting : map_maker =
  let module Tabler = (val tabler overflow: Sigs.TABLER) in
  let module Collection = (val collection replacement accounting: Sigs.COLLECTION) in
  (module Functors.Make_map (Collection) (Tabler))

module type SET_MAKER = functor (H: Hashtbl.HashedType) -> CACHE_SET with type elt = H.t
type set_maker = (module SET_MAKER)

let set_maker ~replacement ~overflow ~accounting : set_maker =
  let module Tabler = (val tabler overflow: Sigs.TABLER) in
  let module Collection = (val collection replacement accounting: Sigs.COLLECTION) in
  (module Functors.Make_set (Collection) (Tabler))
