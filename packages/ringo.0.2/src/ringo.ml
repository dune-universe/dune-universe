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
  | Loose

type accounting =
  | Precise
  | Sloppy

module type CACHE = Sigs.CACHE

module type MAKER =
  functor (H: Hashtbl.HashedType) -> CACHE with type key = H.t
type maker = (module MAKER)

let tabler_of_overflow
: overflow -> (module Sigs.TABLER)
= function
| Strict -> (module Functors.Strict_tabler)
| Loose -> (module Functors.Loose_tabler)

let maker ~replacement ~overflow ~accounting : maker =
  let module Tabler = (val tabler_of_overflow overflow: Sigs.TABLER) in
   match replacement with
  | LRU ->
    let module F = Functors.Make (Functors.LRU_Collection) (Tabler) in
    (module F)
  | FIFO -> match accounting with
    | Precise ->
      let module F = Functors.Make (Functors.FIFO_Precise_Collection) (Tabler) in
      (module F)
    | Sloppy ->
      let module F = Functors.Make (Functors.FIFO_Sloppy_Collection) (Tabler) in
      (module F)
