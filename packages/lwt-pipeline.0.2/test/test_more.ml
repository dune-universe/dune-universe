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

open Pipeline
open Lwt.Infix

let mk_func () =
   let p = 1000 - Random.int 2000 in
   let t = 1000 - Random.int 2000 in
   fun x -> (x + p) * t

let mk_funcs () =
   let rec loop n acc =
      if n <= 0 then
         acc
      else
         loop (n - 1) (mk_func () :: acc)
   in
   loop (Random.int 200) []

let compose_funcs fs =
   let rec loop = function
      | [] -> fun x -> x
      | f :: fs -> fun x -> loop fs (f x)
   in
   loop fs

let lwtise_func f =
   let b = Random.bool () in
   if b then
      fun x -> Lwt.return (f x)
   else
      fun x -> Lwt_unix.sleep (Random.float 0.01) >>= fun () -> Lwt.return (f x)

let mk_step f =
   match Random.int 3 with
   | 0 -> sync f
   | 1 -> async_s (lwtise_func f)
   | 2 -> async_p (lwtise_func f)
   | _ -> assert false

let mk_pipeline ss =
   let rec loop ss =
      match ss with
      | [] -> nil
      | s :: ss -> cons s @@ loop ss
   in
   loop ss

let mk_pipe fs =
   mk_pipeline (List.map mk_step fs)

let mk_input () =
   let rec loop n acc =
      if n <= 0 then
         acc
      else
         loop (n - 1) ((Random.int 10_000) :: acc)
   in
   loop (Random.int 200) []


let run_one_test () =
   let inputs = mk_input () in
   let funcs = mk_funcs () in
   let func = compose_funcs funcs in
   let pipe = mk_pipe funcs in
   run pipe inputs >>= fun result ->
      if result = List.map func inputs then
         Lwt.return_unit
      else
         assert false

let run_n_tests n =
   let rec loop n =
      if n <= 0 then
         Lwt.return_unit
      else
         (run_one_test () <&> loop (n - 1))
   in
   loop n

let () = Lwt_main.run (run_n_tests 1000)
