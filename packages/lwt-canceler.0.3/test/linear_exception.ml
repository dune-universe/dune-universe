(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Lwt.Infix

(* tests that exceptions are not duplicated *)

let witness = ref true
let handle = function
   | [] -> ()
   | [Not_found] ->
      assert !witness; (* checking we're the first *)
      witness := false (* setting a trap for anyone else *)
   | _ -> assert false

let t = Lwt_canceler.create ()

let () = Lwt_canceler.on_cancel t (fun () -> Lwt.return_unit)
let () = Lwt_canceler.on_cancel t (fun () -> Lwt.pause ())
let () = Lwt_canceler.on_cancel t (fun () -> Lwt.pause () >>= fun () -> raise Not_found)
let () = Lwt_canceler.on_cancel t (fun () -> Lwt.pause ())

let () =
   let p1 =
      Lwt.pause () >>= fun () ->
      Lwt_canceler.cancel t >>= function
      | Ok () -> assert false
      | Error excs -> handle excs ; Lwt.return_unit
   in
   let p2 =
      Lwt.pause () >>= fun () ->
      Lwt_canceler.cancel t >>= function
      | Ok () -> assert false
      | Error excs -> handle excs ; Lwt.return_unit
   in
   let p3 =
      Lwt_canceler.when_canceled t >>= function
         | Ok () -> assert false
         | Error (_ :: _) -> assert false
         | Error [] -> Lwt.return_unit
   in
   Lwt_main.run ( Lwt.join [p1; p2; p3] )

