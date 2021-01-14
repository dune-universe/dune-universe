(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type state =
  | Waiting
  | Canceling
  | Canceled
  | Canceled_with_exception

(* A canceler is made of the following elements:
   - One [state] field
   - Two [Lwt_condition]s to resolve promises waiting on some state changes
   - One [callbacks] field to hold multiple callbacks
   *)

type t = {
  mutable state : state;
  canceling : unit Lwt_condition.t;
  canceled : (unit, exn list) result Lwt_condition.t;
  mutable callbacks : (unit -> unit Lwt.t) list;
}

(* preallocated values *)
let lwt_ok_unit = Lwt.return (Ok ())
let lwt_error_nil = Lwt.return (Error [])

let create () =
  {
    state = Waiting;
    canceled = Lwt_condition.create ();
    canceling = Lwt_condition.create ();
    callbacks = [];
  }

let cancel t =
   match t.state with
   | Canceled_with_exception ->
      lwt_error_nil
   | Canceled ->
      lwt_ok_unit
   | Canceling ->
      Lwt_condition.wait t.canceled
   | Waiting ->
      t.state <- Canceling ;
      Lwt_condition.broadcast t.canceling () ;
      let callbacks = t.callbacks in
      (* Reset the [callbacks] field's content can be collected. *)
      t.callbacks <- [];
      Lwt_list.fold_right_s
        (fun cb excs ->
           Lwt.catch
             (fun () -> cb () >>= fun () -> Lwt.return excs)
             (fun exc -> Lwt.return (exc :: excs)))
        callbacks
        []
        >>= function
      | [] ->
         (* no exceptions raised by any of the callbacks *)
            assert (t.state = Canceling);
            t.state <- Canceled ;
            Lwt_condition.broadcast t.canceled (Ok ()) ;
            lwt_ok_unit
      | (_ :: _) as excs ->
         (* Some exceptions *)
            assert (t.state = Canceling);
            t.state <- Canceled_with_exception ;
            (* NOTE: we return the exception to the first caller (the caller
               that triggered the state change to [Canceling]). But we return an
               empty list of exception to the other callers (the callers that
               attached via [t.canceled]). This is intended to distribute the
               responsibility to deal with exception to one single caller. More
               information is available in the interface. *)
            Lwt_condition.broadcast t.canceled (Error []) ;
            Lwt.return (Error excs)

let on_cancel st cb =
   match st.state with
   | Canceled_with_exception | Canceled | Canceling ->
         ()
   | Waiting ->
    let cbs = st.callbacks in
    st.callbacks <- cb :: cbs

let canceling st =
   match st.state with
  | Waiting ->
        false
  | Canceled_with_exception | Canceled | Canceling ->
        true

let when_canceling st =
   match st.state with
   | Waiting ->
        Lwt_condition.wait st.canceling
         | Canceling | Canceled | Canceled_with_exception ->
     Lwt.return_unit

let canceled st =
   match st.state with
  | Waiting | Canceling ->
      false
  | Canceled_with_exception | Canceled ->
      true

let when_canceled st =
   match st.state with
   | Waiting | Canceling ->
     Lwt_condition.wait st.canceled
   | Canceled ->
         lwt_ok_unit
   | Canceled_with_exception ->
         lwt_error_nil

let state t = t.state
