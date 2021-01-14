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

(* test basic behaviour:
   - all callbacks are called in order,
   - state of canceler evolves as documented.
*)

let t = Lwt_canceler.create ()

let witness = ref 0

let () = Lwt_canceler.on_cancel t
   (fun () ->
      assert (Lwt.state (Lwt_canceler.when_canceling t) = Lwt.Return ());
      assert (Lwt.state (Lwt_canceler.when_canceled t) = Lwt.Sleep);
      assert (Lwt_canceler.canceling t);
      assert (not (Lwt_canceler.canceled t));
      assert (!witness = 0);
      incr witness;
      Lwt.return_unit)
let () = Lwt_canceler.on_cancel t
   (fun () ->
      assert (Lwt.state (Lwt_canceler.when_canceling t) = Lwt.Return ());
      assert (Lwt.state (Lwt_canceler.when_canceled t) = Lwt.Sleep);
      assert (Lwt_canceler.canceling t);
      assert (not (Lwt_canceler.canceled t));
      assert (!witness = 1);
      incr witness;
      Lwt_unix.sleep 0.01)
let () = Lwt_canceler.on_cancel t
   (fun () ->
      assert (Lwt.state (Lwt_canceler.when_canceling t) = Lwt.Return ());
      assert (Lwt.state (Lwt_canceler.when_canceled t) = Lwt.Sleep);
      assert (Lwt_canceler.canceling t);
      assert (not (Lwt_canceler.canceled t));
      assert (!witness = 2);
      incr witness;
      Lwt.return_unit)

let () =
   let open Lwt.Infix in
   Lwt_main.run (

      assert (Lwt.state (Lwt_canceler.when_canceling t) = Lwt.Sleep);
      assert (Lwt.state (Lwt_canceler.when_canceled t) = Lwt.Sleep);
      assert (not (Lwt_canceler.canceling t));
      assert (not (Lwt_canceler.canceled t));

      Lwt_canceler.cancel t >>= function

      | Ok () ->
         assert (Lwt_canceler.canceling t);
         assert (Lwt.state (Lwt_canceler.when_canceling t) = Lwt.Return ());
         (* pause because observing state is unsound within a callback-execution cycle *)
         Lwt.pause () >>= fun () ->
         assert (Lwt_canceler.canceled t);
         assert (Lwt.state (Lwt_canceler.when_canceled t) = Lwt.Return (Ok ()));
         Lwt.return_unit

      | Error _ ->
         assert false
   )
