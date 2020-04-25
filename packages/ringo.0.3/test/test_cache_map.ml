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

let test (module Cache: S.CACHE_MAP with type key = int) =
   let c = Cache.create 5 in
   let () = assert (Cache.length c = 0) in
   let () = assert (Cache.find_opt c 0 = None) in

   let () = Cache.add c 0 "zero" in
   let () = assert (Cache.length c = 1) in
   let () = assert (Cache.find_opt c 0 = Some "zero") in

   let () = Cache.add c 1 "one" in
   let () = assert (Cache.length c = 2) in
   let () = assert (Cache.find_opt c 0 = Some "zero") in
   let () = assert (Cache.find_opt c 1 = Some "one") in

   let () = Cache.add c 1 "un" in
   let () = assert (Cache.length c = 2) in
   let () = assert (Cache.find_opt c 0 = Some "zero") in
   let () = assert (Cache.find_opt c 1 = Some "un") in

   let () = Cache.remove c 1 in
   let () = assert (Cache.length c = 1) in
   let () = assert (Cache.find_opt c 0 = Some "zero") in
   let () = assert (Cache.find_opt c 1 = None) in

   let () = List.iter2 (Cache.add c) [1; 2; 3; 4;] ["ein"; "two"; "tres"; "si";] in
   let () = List.iter2 (Cache.add c) [10; 11; 12; 13; 14;] ["a"; "b"; "c"; "d"; "e";] in
   let () = List.iter2 (Cache.add c) [20; 21; 22; 23; 24;] ["!"; "@"; "#"; "$"; "%";] in
   let () = assert (Cache.length c >= 5) in

   let () = Cache.clear c in
   let () = assert (Cache.length c = 0) in

   ()

let () = test (module F.Make_map (F.LRU_Collection) (F.Strict_tabler) (H))
let () = test (module (val Ringo.map_maker ~replacement:LRU ~overflow:Strict ~accounting:Sloppy) (H))
let () = test (module (val Ringo.map_maker ~replacement:LRU ~overflow:Strict ~accounting:Precise) (H))
let () = test (module F.Make_map (F.LRU_Collection) (F.Weak_tabler) (H))
let () = test (module (val Ringo.map_maker ~replacement:LRU ~overflow:Weak ~accounting:Sloppy) (H))
let () = test (module (val Ringo.map_maker ~replacement:LRU ~overflow:Weak ~accounting:Precise) (H))
let () = test (module F.Make_map (F.FIFO_Sloppy_Collection) (F.Strict_tabler) (H))
let () = test (module (val Ringo.map_maker ~replacement:FIFO ~overflow:Strict ~accounting:Sloppy) (H))
let () = test (module F.Make_map (F.FIFO_Sloppy_Collection) (F.Weak_tabler) (H))
let () = test (module (val Ringo.map_maker ~replacement:FIFO ~overflow:Weak ~accounting:Sloppy) (H))
let () = test (module F.Make_map (F.FIFO_Precise_Collection) (F.Strict_tabler) (H))
let () = test (module (val Ringo.map_maker ~replacement:FIFO ~overflow:Strict ~accounting:Precise) (H))
let () = test (module F.Make_map (F.FIFO_Precise_Collection) (F.Weak_tabler) (H))
let () = test (module (val Ringo.map_maker ~replacement:FIFO ~overflow:Weak ~accounting:Precise) (H))


let test_strict (module Cache: S.CACHE_MAP with type key = int) =
   let c = Cache.create 5 in

   let () = List.iter2 (Cache.add c) [0; 1; 2; 3; 4;] ["z"; "ein"; "two"; "tres"; "si";] in
   let () = assert (Cache.length c = 5) in

   let () = Cache.add c 10 "ten" in
   let () = assert (Cache.length c = 5) in
   let () = assert (Cache.find_opt c 0 = None) in
   let () = assert (
      List.for_all
        (fun k -> Cache.find_opt c k <> None)
        [1; 2; 3; 4; 10;]) in

   let () = Cache.clear c in
   let () = assert (Cache.length c = 0) in

   ()

let () = test_strict (module F.Make_map (F.LRU_Collection) (F.Strict_tabler) (H))
let () = test_strict (module (val Ringo.map_maker ~replacement:LRU ~overflow:Strict ~accounting:Sloppy) (H))
let () = test_strict (module (val Ringo.map_maker ~replacement:LRU ~overflow:Strict ~accounting:Precise) (H))
let () = test_strict (module F.Make_map (F.FIFO_Sloppy_Collection) (F.Strict_tabler) (H))
let () = test_strict (module (val Ringo.map_maker ~replacement:FIFO ~overflow:Strict ~accounting:Sloppy) (H))
let () = test_strict (module F.Make_map (F.FIFO_Precise_Collection) (F.Strict_tabler) (H))
let () = test_strict (module (val Ringo.map_maker ~replacement:FIFO ~overflow:Strict ~accounting:Precise) (H))

let test_strict_lru (module Cache: S.CACHE_MAP with type key = int) =
   let c = Cache.create 5 in

   let () = List.iter2 (Cache.add c) [0; 1; 2; 3; 4;] ["z"; "ein"; "two"; "tres"; "si";] in
   let () = assert (Cache.length c = 5) in

   (* find promotes *)
   let () = match Cache.find_opt c 0 with
     | Some "z" -> ()
     | Some _ -> assert false
     | None -> assert false in

   let () = Cache.add c 10 "ten" in
   let () = assert (Cache.length c = 5) in
   let () = assert (Cache.find_opt c 1 = None) in
   let () = assert (
      List.for_all
        (fun k -> Cache.find_opt c k <> None)
        [0; 2; 3; 4; 10;]) in

   let () = Cache.clear c in
   let () = assert (Cache.length c = 0) in

   ()

let () = test_strict_lru (module F.Make_map (F.LRU_Collection) (F.Strict_tabler) (H))
let () = test_strict_lru (module (val Ringo.map_maker ~replacement:LRU ~overflow:Strict ~accounting:Sloppy) (H))
let () = test_strict_lru (module (val Ringo.map_maker ~replacement:LRU ~overflow:Strict ~accounting:Precise) (H))

let test_strict_fifo_sloppy (module Cache: S.CACHE_MAP with type key = int) =
   let c = Cache.create 5 in

   let () = List.iter2 (Cache.add c) [0; 1; 2; 3; 4;] ["z"; "ein"; "two"; "tres"; "si";] in
   let () = assert (Cache.length c = 5) in

   (* finds has no side-effects *)
   let () = match Cache.find_opt c 0 with
     | Some "z" -> ()
     | Some _ -> assert false
     | None -> assert false in

   let () = Cache.add c 10 "ten" in
   let () = assert (Cache.length c = 5) in
   let () = assert (Cache.find_opt c 0 = None) in
   let () = assert (
      List.for_all
        (fun k -> Cache.find_opt c k <> None)
        [1; 2; 3; 4; 10;]) in

   let () = Cache.clear c in
   let () = assert (Cache.length c = 0) in

   ()

let () = test_strict_fifo_sloppy (module F.Make_map (F.FIFO_Sloppy_Collection) (F.Strict_tabler) (H))
let () = test_strict_fifo_sloppy (module (val Ringo.map_maker ~replacement:FIFO ~overflow:Strict ~accounting:Sloppy) (H))

let test_strict_fifo_precise (module Cache: S.CACHE_MAP with type key = int) =
   let c = Cache.create 5 in

   let () = List.iter2 (Cache.add c) [0; 1; 2; 3; 4;] ["z"; "ein"; "two"; "tres"; "si";] in
   let () = assert (Cache.length c = 5) in

   (* finds has no side-effects *)
   let () = match Cache.find_opt c 0 with
     | Some "z" -> ()
     | Some _ -> assert false
     | None -> assert false in

   let () = Cache.add c 10 "ten" in
   let () = assert (Cache.length c = 5) in
   let () = assert (Cache.find_opt c 0 = None) in
   let () = assert (
      List.for_all
        (fun k -> Cache.find_opt c k <> None)
        [1; 2; 3; 4; 10;]) in

   let () = Cache.remove c 4 in
   let () = assert (Cache.length c = 4) in
   let () = Cache.add c 11 "eleven" in
   let () = assert (Cache.length c = 5) in

   let () = Cache.clear c in
   let () = assert (Cache.length c = 0) in

   ()

let () = test_strict_fifo_precise (module F.Make_map (F.FIFO_Precise_Collection) (F.Strict_tabler) (H))
let () = test_strict_fifo_precise (module (val Ringo.map_maker ~replacement:FIFO ~overflow:Strict ~accounting:Precise) (H))

