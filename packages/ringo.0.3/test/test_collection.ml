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

module Sigs = Ringo__Sigs
module Functors = Ringo__Functors

let test_collection (module Collection : Sigs.COLLECTION) =
   let eq_box b1 b2 = Collection.data b1 = Collection.data b2 in
   let eq_box_list l1 l2 = List.for_all2 eq_box l1 l2 in

   let c = Collection.create 5 in

   let () = assert (eq_box_list [] @@ Collection.elements c) in

   let node_00 = Collection.add c "00" in

   let () = assert (Collection.data node_00 = "00") in
   let () = assert (eq_box_list [node_00] @@ Collection.elements c) in

   let node_01 = Collection.add c "01" in
   let () = assert (eq_box_list [node_00; node_01;] @@ Collection.elements c) in
   let node_02 = Collection.add c "02" in
   let node_03 = Collection.add c "03" in
   let node_04 = match Collection.add_and_return_erased c "04" with
      | (node_04, None) -> node_04
      | (_, Some _) -> assert false in

   let () = assert (eq_box_list [node_00; node_01; node_02; node_03; node_04;] @@ Collection.elements c) in

   let node_10 = Collection.add c "10" in

   let () = assert (Collection.data node_10 = "10") in
   let () = assert (eq_box_list [node_01; node_02; node_03; node_04; node_10] @@ Collection.elements c) in

   let node_11 = match Collection.add_and_return_erased c "11" with
      | (node_11, Some "01") -> node_11
      | (_, Some _) -> assert false
      | (_, None) -> assert false in

   let () = assert (eq_box_list [node_02; node_03; node_04; node_10; node_11;] @@ Collection.elements c) in

   let nodes = Collection.add_list c [ "garbage"; "trash"; "junk"; "20"; "21"; "22"; "23"; "24"] in

   let () = assert (eq_box_list nodes @@ Collection.elements c) in
   let () = assert (List.map Collection.data nodes = ["20"; "21"; "22"; "23"; "24";]) in
   let () = assert (List.map Collection.data (Collection.elements c) = ["20"; "21"; "22"; "23"; "24";]) in

   let single_node = Collection.add_list c [ "single" ] in

   let () = assert (List.map Collection.data single_node = ["single"]) in

   let () = Collection.clear c in

   let () = assert (eq_box_list [] @@ Collection.elements c) in

   ()

let () = test_collection (module Functors.LRU_Collection)
let () = test_collection (module Functors.FIFO_Sloppy_Collection)
let () = test_collection (module Functors.FIFO_Precise_Collection)

let test_collection_singleton (module Collection : Sigs.COLLECTION) =
   let eq_box b1 b2 = Collection.data b1 = Collection.data b2 in
   let eq_box_list l1 l2 = List.for_all2 eq_box l1 l2 in

   let c = Collection.create 1 in

   let () = assert (eq_box_list [] @@ Collection.elements c) in

   let node_00 = Collection.add c "00" in

   let () = assert (Collection.data node_00 = "00") in
   let () = assert (eq_box_list [node_00] @@ Collection.elements c) in

   let (node_01, pops) = Collection.add_and_return_erased c "01" in

   let () = assert (pops = Some "00") in
   let () = assert (eq_box_list [node_01;] @@ Collection.elements c) in

   let () = Collection.clear c in

   let () = assert (eq_box_list [] @@ Collection.elements c) in

   ()

let () = test_collection_singleton (module Functors.LRU_Collection)
let () = test_collection_singleton (module Functors.FIFO_Sloppy_Collection)
let () = test_collection_singleton (module Functors.FIFO_Precise_Collection)

(* Now testing the remove in precise collection *)
let test_precise_remove (module Collection : Sigs.COLLECTION) =
   let c = Collection.create 5 in

   let nodes = Collection.add_list c [ "garbage"; "trash"; "junk"; "20"; "21"; "22"; "23"; "24"] in
   let (node_20, _, node_22, _, node_24) = match nodes with
      | [node_20; node_21; node_22; node_23; node_24] -> (node_20, node_21, node_22, node_23, node_24)
      | _ -> assert false in
   let () = assert (Collection.data node_22 = "22") in

   let () = Collection.remove c node_22 in

   let () = assert (List.map Collection.data (Collection.elements c) = ["20"; "21"; "23"; "24";]) in

   let () = Collection.remove c node_20 in

   let () = assert (List.map Collection.data (Collection.elements c) = ["21"; "23"; "24";]) in

   let () = Collection.remove c node_24 in

   let () = assert (List.map Collection.data (Collection.elements c) = ["21"; "23";]) in

   ()

let () = test_precise_remove (module Functors.LRU_Collection)
let () = test_precise_remove (module Functors.FIFO_Precise_Collection)


(* Now testing specific policies, specifically, testing the effect of promote *)

let test_lru () =
   let module Collection = Functors.LRU_Collection in
   let eq_box b1 b2 = Collection.data b1 = Collection.data b2 in
   let eq_box_list l1 l2 = List.for_all2 eq_box l1 l2 in

   let c = Collection.create 5 in

   let node_00 = Collection.add c "00" in
   let node_01 = Collection.add c "01" in
   let node_02 = Collection.add c "02" in
   let node_03 = Collection.add c "03" in
   let node_04 = Collection.add c "04" in

   let node_10 = match Collection.add_and_return_erased c "10" with
      | (node_11, Some "00") -> node_11
      | (_, Some _) -> assert false
      | (_, None) -> assert false in

   let () = Collection.promote c node_01 in

   let node_11 = match Collection.add_and_return_erased c "11" with
      | (node_11, Some "02") -> node_11
      | (_, Some _) -> assert false
      | (_, None) -> assert false in

   let () = assert (eq_box_list [node_03; node_04; node_10; node_01; node_11;] @@ Collection.elements c) in

   let () = Collection.remove c node_11 in
   let node_12 = match Collection.add_and_return_erased c "12" with
      | (node_12, None) -> node_12 (* elements are counted precisely *)
      | (_, Some _) -> assert false in

   let () = Collection.clear c in

   let () = assert (eq_box_list [] @@ Collection.elements c) in

   ignore node_00; ignore node_02; ignore node_12;
   ()

let () = test_lru ()

let test_fifo_sloppy () =
   let module Collection = Functors.FIFO_Sloppy_Collection in
   let eq_box b1 b2 = Collection.data b1 = Collection.data b2 in
   let eq_box_list l1 l2 = List.for_all2 eq_box l1 l2 in

   let c = Collection.create 5 in

   let node_00 = Collection.add c "00" in
   let node_01 = Collection.add c "01" in
   let node_02 = Collection.add c "02" in
   let node_03 = Collection.add c "03" in
   let node_04 = Collection.add c "04" in

   let node_10 = match Collection.add_and_return_erased c "10" with
      | (node_11, Some "00") -> node_11
      | (_, Some _) -> assert false
      | (_, None) -> assert false in

   (* promote is a no-op in FIFO *)
   let () = Collection.promote c node_01 in

   let node_11 = match Collection.add_and_return_erased c "11" with
      | (node_11, Some "01") -> node_11
      | (_, Some _) -> assert false
      | (_, None) -> assert false in

   let () = assert (eq_box_list [node_02; node_03; node_04; node_10; node_11;] @@ Collection.elements c) in

   let () = Collection.remove c node_11 in
   let node_12 = match Collection.add_and_return_erased c "12" with
      | (node_12, Some "02") -> node_12 (* elements are counted sloppily *)
      | (_, Some _) -> assert false
      | (_, None) -> assert false in

   let () = Collection.clear c in

   let () = assert (eq_box_list [] @@ Collection.elements c) in

   ignore node_00; ignore node_01; ignore node_02; ignore node_12;
   ()

let () = test_fifo_sloppy ()

let test_fifo_precise () =
   let module Collection = Functors.FIFO_Precise_Collection in
   let eq_box b1 b2 = Collection.data b1 = Collection.data b2 in
   let eq_box_list l1 l2 = List.for_all2 eq_box l1 l2 in

   let c = Collection.create 5 in

   let node_00 = Collection.add c "00" in
   let node_01 = Collection.add c "01" in
   let node_02 = Collection.add c "02" in
   let node_03 = Collection.add c "03" in
   let node_04 = Collection.add c "04" in

   let node_10 = match Collection.add_and_return_erased c "10" with
      | (node_11, Some "00") -> node_11
      | (_, Some _) -> assert false
      | (_, None) -> assert false in

   (* promote is a no-op in FIFO *)
   let () = Collection.promote c node_01 in

   let node_11 = match Collection.add_and_return_erased c "11" with
      | (node_11, Some "01") -> node_11
      | (_, Some _) -> assert false
      | (_, None) -> assert false in

   let () = assert (eq_box_list [node_02; node_03; node_04; node_10; node_11;] @@ Collection.elements c) in

   let () = Collection.remove c node_11 in
   let node_12 = match Collection.add_and_return_erased c "12" with
      | (node_12, None) -> node_12 (* elements are counted precisely *)
      | (_, Some _) -> assert false in


   ignore node_00; ignore node_01; ignore node_02; ignore node_12;
   ()

let () = test_fifo_precise ()
