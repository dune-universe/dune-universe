(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, Inc. <contact@nomadic-labs.com>          *)
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

let l n =
  let array = Array.init n Fun.id in
  Array.to_list array

let lengths = [0; 1; 2; 16; 128; 4096; 2_000_000]

(* NOTE: this test shows that the function is applied to the elements of the
   list in *reversed* order. *)
let test l =
  let aref = ref [] in
  let b =
    List_map.map_pure
      (fun x ->
        aref := x :: !aref ;
        x)
      l
  in
  ignore b ;
  let a = !aref in
  a = l

let () = print_endline "side-effect order in tail-rec list map"

let result = List.for_all (fun length -> test (l length)) lengths

let () =
  if result then print_endline "side-effect order in tail-rec list map: SUCCESS"
  else print_endline "side-effect order in tail-rec list map: FAILURE"
