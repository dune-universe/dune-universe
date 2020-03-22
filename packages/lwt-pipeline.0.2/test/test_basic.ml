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

let inputs = [
   [];
   [0;];
   [0;0];
   [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;];
   [0;1;2;3;];
   [0;1;0;1;];
   [min_int; max_int;];
   [0;1;2;3;4;5;6;7;8;9;0;1;2;3;4;5;6;7;8;];
]

let pipes_funcs = [
   (nil, (fun x -> x));
   (cons (sync succ) @@ nil, succ);
   (cons (async_p (fun x -> Lwt.return (succ x))) @@ nil, succ);
   (cons (async_s (fun x -> Lwt.return (succ x))) @@ nil, succ);
   (cons (sync (fun _ -> 0)) @@ nil, (fun _ -> 0));
   (cons (async_p (fun x -> Lwt_unix.sleep 0.001 >>= fun () -> Lwt.return x)) @@ nil,
    (fun x -> x));
   (cons (async_p (fun x -> Lwt_unix.sleep 0.001 >>= fun () -> Lwt.return x))
    @@ cons (sync (fun x -> x))
    @@ cons (async_s (fun x -> Lwt.return x))
    @@ nil,
    (fun x -> x));
   (cons (async_p (fun x -> Lwt_unix.sleep (Random.float 0.1) >>= fun () -> Lwt.return x))
    @@ cons (sync (fun x -> 0 - x))
    @@ cons (async_s (fun x -> Lwt.return (0 - x)))
    @@ nil,
    (fun x -> (0 - (0 - x))));
]

let () =
   Lwt_main.run @@
   Lwt_list.iter_s
     (fun input ->
        Lwt_list.iter_s
         (fun (pipeline, func) ->
            run pipeline input >>= fun pipe_result ->
               if List.map func input = pipe_result then
                  Lwt.return_unit
               else
                  assert false)
         pipes_funcs
         )
     inputs
