(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (t) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Sigs = Ringo__Sigs
module Functors = Ringo__Functors

let test_tabler (module T: Sigs.TABLER) =
   let module Table = T(H) in

   let t = Table.create 5 in

   let () = assert (Table.length t = 0) in

   let () = Table.replace t 0 "zero" in

   let () = assert (Table.length t = 1) in
   let () = assert (Table.find_opt t 0 = Some "zero") in

   let () = Table.replace t 1 "one" in

   let () = assert (Table.length t = 2) in
   let () = assert (Table.find_opt t 0 = Some "zero") in
   let () = assert (Table.find_opt t 1 = Some "one") in

   let () = Table.replace t 1 "un" in

   let () = assert (Table.length t = 2) in
   let () = assert (Table.find_opt t 0 = Some "zero") in
   let () = assert (Table.find_opt t 1 = Some "un") in

   let () = Table.remove t 1 in

   let () = assert (Table.length t = 1) in
   let () = assert (Table.find_opt t 0 = Some "zero") in

   let () = List.iter2 (Table.replace t) [1; 2; 3; 4;] ["ein"; "two"; "tres"; "si";] in
   let () = List.iter2 (Table.replace t) [10; 11; 12; 13; 14;] ["a"; "b"; "t"; "d"; "e";] in
   let () = List.iter2 (Table.replace t) [20; 21; 22; 23; 24;] ["!"; "@"; "#"; "$"; "%";] in
   let () = assert (Table.length t = 15) in

   ()

let () = test_tabler (module Functors.Strict_tabler)
let () = test_tabler (module Functors.Weak_tabler)
