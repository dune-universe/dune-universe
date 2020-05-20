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

module H : Hashtbl.HashedType with type t = int = struct
   type t = int
   let equal = (=)
   let hash = Hashtbl.hash
end

module S = Ringo__Sigs
module F = Ringo__Functors

let test (module Cache: S.CACHE_SET with type elt = int) =
   let c = Cache.create 5 in

   let () = assert (Cache.length c = 0) in
   let () = assert (Cache.mem c 0 = false) in
   let () = assert (Cache.mem c 1 = false) in

   let () = Cache.add c 0 in
   let () = assert (Cache.length c = 1) in
   let () = assert (Cache.mem c 0 = true) in
   let () = assert (Cache.mem c 1 = false) in

   let () = Cache.add c 1 in
   let () = assert (Cache.length c = 2) in
   let () = assert (Cache.mem c 0 = true) in
   let () = assert (Cache.mem c 1 = true) in

   let () = Cache.add c 1 in
   let () = assert (Cache.length c = 2) in
   let () = assert (Cache.mem c 0 = true) in
   let () = assert (Cache.mem c 1 = true) in

   let () = Cache.remove c 1 in
   let () = assert (Cache.length c = 1) in
   let () = assert (Cache.mem c 0 = true) in
   let () = assert (Cache.mem c 1 = false) in

   let () = List.iter (Cache.add c) [1; 2; 3; 4;] in
   let () = List.iter (Cache.add c) [10; 11; 12; 13; 14;] in
   let () = List.iter (Cache.add c) [20; 21; 22; 23; 24;] in
   let () = assert (Cache.length c >= 5) in

   let () = Cache.clear c in
   let () = assert (Cache.length c = 0) in

   ()

let () = test (module F.Make_set (F.LRU_Collection) (F.Strong_tabler) (H))
let () = test (module (val Ringo.set_maker ~replacement:LRU ~overflow:Strong ~accounting:Sloppy) (H))
let () = test (module (val Ringo.set_maker ~replacement:LRU ~overflow:Strong ~accounting:Precise) (H))
let () = test (module F.Make_set (F.LRU_Collection) (F.Weak_tabler) (H))
let () = test (module (val Ringo.set_maker ~replacement:LRU ~overflow:Weak ~accounting:Sloppy) (H))
let () = test (module (val Ringo.set_maker ~replacement:LRU ~overflow:Weak ~accounting:Precise) (H))
let () = test (module F.Make_set (F.FIFO_Sloppy_Collection) (F.Strong_tabler) (H))
let () = test (module (val Ringo.set_maker ~replacement:FIFO ~overflow:Strong ~accounting:Sloppy) (H))
let () = test (module F.Make_set (F.FIFO_Sloppy_Collection) (F.Weak_tabler) (H))
let () = test (module (val Ringo.set_maker ~replacement:FIFO ~overflow:Weak ~accounting:Sloppy) (H))
let () = test (module F.Make_set (F.FIFO_Precise_Collection) (F.Strong_tabler) (H))
let () = test (module (val Ringo.set_maker ~replacement:FIFO ~overflow:Strong ~accounting:Precise) (H))
let () = test (module F.Make_set (F.FIFO_Precise_Collection) (F.Weak_tabler) (H))
let () = test (module (val Ringo.set_maker ~replacement:FIFO ~overflow:Weak ~accounting:Precise) (H))


let test_strict (module Cache: S.CACHE_SET with type elt = int) =
   let c = Cache.create 5 in

   let () = List.iter (Cache.add c) [0; 1; 2; 3; 4;] in
   let () = assert (Cache.length c = 5) in

   let () = Cache.add c 10 in
   let () = assert (Cache.length c = 5) in
   let () = assert (Cache.mem c 0 = false) in
   let () = assert (List.for_all (fun v -> Cache.mem c v = true) [1; 2; 3; 4; 10;]) in

   let () = Cache.clear c in
   let () = assert (Cache.length c = 0) in

   ()

let () = test_strict (module F.Make_set (F.LRU_Collection) (F.Strong_tabler) (H))
let () = test_strict (module (val Ringo.set_maker ~replacement:LRU ~overflow:Strong ~accounting:Sloppy) (H))
let () = test_strict (module (val Ringo.set_maker ~replacement:LRU ~overflow:Strong ~accounting:Precise) (H))
let () = test_strict (module F.Make_set (F.FIFO_Sloppy_Collection) (F.Strong_tabler) (H))
let () = test_strict (module (val Ringo.set_maker ~replacement:FIFO ~overflow:Strong ~accounting:Sloppy) (H))
let () = test_strict (module F.Make_set (F.FIFO_Precise_Collection) (F.Strong_tabler) (H))
let () = test_strict (module (val Ringo.set_maker ~replacement:FIFO ~overflow:Strong ~accounting:Precise) (H))


let test_precise (module Cache: S.CACHE_SET with type elt = int) =
   let c = Cache.create 5 in

   let () = List.iter (Cache.add c) [0; 1; 2; 3; 4;] in
   let () = assert (Cache.length c = 5) in

   let () = Cache.add c 0 in
   let () = assert (Cache.length c = 5) in

   let () = List.iter (Cache.add c) [0;0;0;0;1;0;0;4;4;4;4;0;0;1;0;0;4;4;4;4] in
   let () = assert (Cache.length c = 5) in

   ()

let () = test_precise (module F.Make_set (F.LRU_Collection) (F.Strong_tabler) (H))
let () = test_precise (module (val Ringo.set_maker ~replacement:LRU ~overflow:Strong ~accounting:Precise) (H))
let () = test_precise (module F.Make_set (F.LRU_Collection) (F.Weak_tabler) (H))
let () = test_precise (module (val Ringo.set_maker ~replacement:LRU ~overflow:Weak ~accounting:Precise) (H))
let () = test_precise (module F.Make_set (F.FIFO_Precise_Collection) (F.Strong_tabler) (H))
let () = test_precise (module (val Ringo.set_maker ~replacement:FIFO ~overflow:Strong ~accounting:Precise) (H))
let () = test_precise (module F.Make_set (F.FIFO_Precise_Collection) (F.Weak_tabler) (H))
let () = test_precise (module (val Ringo.set_maker ~replacement:FIFO ~overflow:Weak ~accounting:Precise) (H))


let test_strict_lru (module Cache: S.CACHE_SET with type elt = int) =
   let c = Cache.create 5 in

   let () = List.iter (Cache.add c) [0; 1; 2; 3; 4;] in
   let () = assert (Cache.length c = 5) in

   (* find promotes *)
   let () = match Cache.mem c 0 with
     | true -> ()
     | false -> assert false in

   let () = Cache.add c 10 in
   let () = assert (Cache.length c = 5) in
   let () = assert (Cache.mem c 1 = false) in
   let () = assert (List.for_all (fun v -> Cache.mem c v = true) [0; 2; 3; 4; 10;]) in

   let () = Cache.clear c in
   let () = assert (Cache.length c = 0) in

   ()

let () = test_strict_lru (module F.Make_set (F.LRU_Collection) (F.Strong_tabler) (H))
let () = test_strict_lru (module (val Ringo.set_maker ~replacement:LRU ~overflow:Strong ~accounting:Sloppy) (H))
let () = test_strict_lru (module (val Ringo.set_maker ~replacement:LRU ~overflow:Strong ~accounting:Precise) (H))

let test_strict_fifo_sloppy (module Cache: S.CACHE_SET with type elt = int) =
   let c = Cache.create 5 in

   let () = List.iter (Cache.add c) [0; 1; 2; 3; 4;] in
   let () = assert (Cache.length c = 5) in

   (* finds has no side-effects *)
   let () = match Cache.mem c 0 with
     | true -> ()
     | false -> assert false in

   let () = Cache.add c 10 in
   let () = assert (Cache.length c = 5) in
   let () = assert (Cache.mem c 0 = false) in
   let () = assert (List.for_all (fun v -> Cache.mem c v = true) [1; 2; 3; 4; 10;]) in

   let () = Cache.clear c in
   let () = assert (Cache.length c = 0) in

   ()

let () = test_strict_fifo_sloppy (module F.Make_set (F.FIFO_Sloppy_Collection) (F.Strong_tabler) (H))
let () = test_strict_fifo_sloppy (module (val Ringo.set_maker ~replacement:FIFO ~overflow:Strong ~accounting:Sloppy) (H))

let test_strict_fifo_precise (module Cache: S.CACHE_SET with type elt = int) =
   let c = Cache.create 5 in

   let () = List.iter (Cache.add c) [0; 1; 2; 3; 4;] in
   let () = assert (Cache.length c = 5) in

   (* finds has no side-effects *)
   let () = match Cache.mem c 0 with
     | true -> ()
     | false -> assert false in

   let () = Cache.add c 10 in
   let () = assert (Cache.length c = 5) in
   let () = assert (Cache.mem c 0 = false) in
   let () = assert (List.for_all (fun v -> Cache.mem c v = true) [1; 2; 3; 4; 10;]) in

   let () = Cache.remove c 4 in
   let () = assert (Cache.length c = 4) in
   let () = Cache.add c 11 in
   let () = assert (Cache.length c = 5) in

   let () = Cache.clear c in
   let () = assert (Cache.length c = 0) in

   ()

let () = test_strict_fifo_precise (module F.Make_set (F.FIFO_Precise_Collection) (F.Strong_tabler) (H))
let () = test_strict_fifo_precise (module (val Ringo.set_maker ~replacement:FIFO ~overflow:Strong ~accounting:Precise) (H))

