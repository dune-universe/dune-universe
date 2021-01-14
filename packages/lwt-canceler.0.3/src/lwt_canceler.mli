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

(** {1 [Lwt_canceler]}

    Lwt-canceler is a library for synchronizing cancellation of tasks and
    clean-up of associated resources in Lwt. *)

(** {2 States and transitions} *)

(** A [Lwt_canceler.t] is a synchronization object that can be in either of
    three {!state}s: {e waiting}, {e canceling}, and {e canceled}.

    - When the synchronization object is {e waiting}, you can attach callbacks
      using {!on_cancel}.
    - When the synchronization object is {e waiting}, you can trigger the
      cancellation using {!cancel}, doing so changes the state of the
      synchronization object into {e canceling} and triggers the execution
      of the attached callbacks.
    - When the callbacks have finished executing, the synchronization object's
      state changes to {e canceled}
*)
type t

(** The [state] of a canceler. It is as described in the documentation of the
    type {!t}. The state [Canceled_with_exception] indicates that the canceler
    has been canceled but that exceptions have been raised during execution of
    the callbacks.

    The exceptions raised during the execution of the callbacks are only
    available to the first caller of {!cancel}. The reasons for this are
    detailed in the documentation of {!cancel}.
    *)
type state =
  | Waiting
  | Canceling
  | Canceled
  | Canceled_with_exception

(** [create ()] returns a canceler in {e waiting} state. *)
val create : unit -> t

(** [cancel t] triggers the cancellation process and returns a promise [p]:

    {ol
      {li [t] changes state to {e canceling},}
      {li the callbacks attached to [t] execute sequentially in the order they
          were attached,}
      {li when the callbacks have all been executed, [t] changes state to
         {e canceled} and [p] resolves.}
    }

    If [t] is in {e canceling} state, [cancel t] returns a promise that resolves
    when [t] changes state to {e canceled}. The call has no side-effects.

    If [t] is already in {e canceled} state, [cancel t] returns an already
    resolved promise.

    If all the callbacks execute without raising exceptions and their promises
    are resolved successfully, then the promise returned by [cancel] resolves
    successfully with [Ok ()].

    If one or more of the attached callbacks raise exceptions or return promises
    that become rejected, the promise returned by one of the caller of [cancel]
    resolves to [Error excs] where [excs] is the (non-empty) list of exceptions
    that were raised. The promise returns by the other callers of [cancel]
    return a promise that resolve to [Error []]. This result indicates to
    these other callers that errors occurred but that they are not responsible
    for handling them.

    The aim of this error management strategy is to ensure that errors are
    treated only once: one caller is responsible for handling the errors
    (closing some open file descriptors, freeing a lock, etc.), the others
    callers are not.
*)
val cancel : t -> (unit, exn list) result Lwt.t

(** {2 Callbacks} *)

(** [on_cancel t callback], if [t] is in {e waiting} state, attaches [callback]
    to [t].

    When [t] becomes {e canceling}, the callbacks are called sequentially, in
    FIFO order (meaning that the last callback added is executed last).

    If one of the callback fails (i.e., if the promise returned by one of the
    callbacks is rejected), the following callbacks are executed nonetheless.

    If one or more of the callbacks fail, then the first call to {!cancel}
    returns [Error _] as described above.

    If [t] is in {e canceling} or {e canceled} state, [on_cancel t] is a no-op.
    *)
val on_cancel : t -> (unit -> unit Lwt.t) -> unit

(** {2 State inspection} *)

(** [canceling t] is [true] iff [t] is {e canceled} or {e canceling}. *)
val canceling : t -> bool

(** [when_canceling t] is a promise that is fulfilled when [t] enters the
    {e canceling} state. If [t] is already in the {e canceling} or {e canceled}
    state, then the promise is already fulfilled. *)
val when_canceling : t -> unit Lwt.t

(** [canceled t] is [true] iff [t] is {e canceled}. *)
val canceled : t -> bool

(** [when_canceled t] is a promise that is fulfilled when [t] enters the
    {e canceled} state. If [t] is already in the {e canceled} state, then the
    promise is already fulfilled.

    It is fulfilled with the value [Ok ()] if the callbacks attached to [t] did
    not raise any exception, it is fulfilled with [Error []] otherwise. Check
    {!cancel} for additional information about error management. *)
val when_canceled : t -> (unit, exn list) result Lwt.t

(** [state t] is the state of [t] at the moment of call. *)
val state : t -> state
