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

(* this is the same as test_cache, but for the [Ringo.maker] interface. *)

module H : Hashtbl.HashedType with type t = int = struct
   type t = int
   let equal = (=)
   let hash = Hashtbl.hash
end

let test_cache (module Maker: Ringo.MAKER) =
   let module Cache = Maker(H) in

   let c = Cache.create 5 in

   let () = assert (Cache.length c = 0) in

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

   let () = List.iter2 (Cache.add c) [1; 2; 3; 4;] ["ein"; "two"; "tres"; "si";] in
   let () = List.iter2 (Cache.add c) [10; 11; 12; 13; 14;] ["a"; "b"; "c"; "d"; "e";] in
   let () = List.iter2 (Cache.add c) [20; 21; 22; 23; 24;] ["!"; "@"; "#"; "$"; "%";] in
   let () = assert (Cache.length c >= 5) in

   ()

let () = test_cache (Ringo.maker ~replacement:LRU ~overflow:Strict ~accounting:Precise)
let () = test_cache (Ringo.maker ~replacement:LRU ~overflow:Strict ~accounting:Sloppy)
let () = test_cache (Ringo.maker ~replacement:LRU ~overflow:Loose ~accounting:Precise)
let () = test_cache (Ringo.maker ~replacement:LRU ~overflow:Loose ~accounting:Sloppy)
let () = test_cache (Ringo.maker ~replacement:FIFO ~overflow:Strict ~accounting:Precise)
let () = test_cache (Ringo.maker ~replacement:FIFO ~overflow:Strict ~accounting:Sloppy)
let () = test_cache (Ringo.maker ~replacement:FIFO ~overflow:Loose ~accounting:Precise)
let () = test_cache (Ringo.maker ~replacement:FIFO ~overflow:Loose ~accounting:Sloppy)

let test_strict_cache (module Maker: Ringo.MAKER) =
   let module Cache = Maker(H) in

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

   ()

let () = test_strict_cache (Ringo.maker ~replacement:LRU ~overflow:Strict ~accounting:Precise)
let () = test_strict_cache (Ringo.maker ~replacement:LRU ~overflow:Strict ~accounting:Sloppy)
let () = test_strict_cache (Ringo.maker ~replacement:FIFO ~overflow:Strict ~accounting:Precise)
let () = test_strict_cache (Ringo.maker ~replacement:FIFO ~overflow:Strict ~accounting:Sloppy)

let test_strict_lru_cache (module Maker: Ringo.MAKER) =
   let module Cache = Maker(H) in

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

   ()

let () = test_strict_lru_cache (Ringo.maker ~replacement:LRU ~overflow:Strict ~accounting:Precise)
let () = test_strict_lru_cache (Ringo.maker ~replacement:LRU ~overflow:Strict ~accounting:Sloppy)

let test_strict_fifo_sloppy_cache (module Maker: Ringo.MAKER) =
   let module Cache = Maker(H) in

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

   ()

let () = test_strict_fifo_sloppy_cache (Ringo.maker ~replacement:FIFO ~overflow:Strict ~accounting:Sloppy)

let test_strict_fifo_precise_cache (module Maker: Ringo.MAKER) =
   let module Cache = Maker(H) in

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

   ()

let () = test_strict_fifo_precise_cache (Ringo.maker ~replacement:FIFO ~overflow:Strict ~accounting:Precise)
