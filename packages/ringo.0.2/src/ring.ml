(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type 'a raw = Empty of int | Inited of {data : 'a array; mutable pos : int}

type 'a t = 'a raw ref

let create size =
  if size <= 0 then invalid_arg "Ring.create: size must be positive"
  else ref (Empty size)

let capacity r =
  match !r with
  | Empty i -> i
  | Inited {data;_} -> Array.length data

let add r v =
  match !r with
  | Empty size ->
      r := Inited {data = Array.make size v; pos = 0}
  | Inited s ->
      s.pos <-
        ( if s.pos = (2 * Array.length s.data) - 1 then Array.length s.data
        else s.pos + 1 ) ;
      s.data.(s.pos mod Array.length s.data) <- v

let add_and_return_erased r v =
  let replaced =
    match !r with
    | Empty _ ->
        None
    | Inited s ->
        if s.pos >= Array.length s.data - 1 then
          Some s.data.((s.pos + 1) mod Array.length s.data)
        else None
  in
  add r v ; replaced

let clear r =
  match !r with
  | Empty _ ->
      ()
  | Inited {data; _} ->
      r := Empty (Array.length data)

let add_list r l = List.iter (add r) l

let fold r ~init ~f =
  match !r with
  | Empty _ ->
      init
  | Inited {data; pos} ->
      let size = Array.length data in
      let acc = ref init in
      for i = 0 to min pos (size - 1) do
        acc := f !acc data.((pos - i) mod size)
      done ;
      !acc

let elements t = fold t ~init:[] ~f:(fun acc elt -> elt :: acc)
