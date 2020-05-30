(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019,2020 DaiLambda, Inc. <contact@dailambda.jp>            *)
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
include Random.State

let char rs = Char.chr @@ int rs 256

let hex rs n =
  String.concat ""
  @@ List.init n @@ fun _ -> 
  Printf.sprintf "%x" @@ int rs 16

let binary rs n = String.init n (fun _ -> char rs)

let one_of rs xs =
  let n = Array.length xs in
  Array.unsafe_get xs @@ int rs n
  
let lower =
  let chars = Array.init 32 (fun i ->
      if i < 26 then Char.chr @@ Char.code 'a' + i
      else match i - 26 with
        | 0 -> ' '
        | 1 -> '-'
        | 2 -> '.'
        | 3 -> ':'
        | 4 -> '='
        | 5 -> '_'
        | _ -> assert false)
  in
  fun rs n ->
    String.init n (fun _ -> one_of rs chars)

let alphanum =
  let chars = Array.init 64 (fun i ->
      if i < 10 then Char.chr @@ Char.code '0' + i
      else 
        let i = i - 10 in
        if i < 26 then Char.chr @@ Char.code 'A' + i
        else 
          let i = i - 26 in
          if i < 26 then Char.chr @@ Char.code 'a' + i
          else 
            let i = i - 26 in
            match i with
            | 0 -> '-'
            | 1 -> '_'
            | _ -> assert false)
  in
  fun rs n ->
    String.init n (fun _ -> one_of rs chars)

let ascii rs n =
  String.init n (fun _ -> Char.chr @@ int rs 128)
