(******************************************************************************)
(*                                                                            *)
(*                                  Monolith                                  *)
(*                                                                            *)
(*                              François Pottier                              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

(* All global state must in principle be registered here, so it can be properly
   re-initialized at the beginning of each new run. *)

(* [register save] registers a function [save] that is in charge of saving a
   piece of the global state. The function call [save()] must save the current
   state and return a function [reset] that restores this saved state. *)
val register: (unit -> unit -> unit) -> unit

(* [save()] executes all of the [save] functions that have been registered. *)
val save: unit -> unit

(* [reset()] executes all of the [reset] functions that have been obtained
   as a result of calling [save()]. *)
val reset: unit -> unit

(* The basic usage pattern is [register* ; save ; reset*]. That is, [save]
   must called once. [reset] can be called only after [save] has been called.
   [register] must be called before [save] has been called. *)

(* For greater flexibility, we actually allow [register] to be called after
   [save] has been called. Such a call has no effect; indeed, if a piece of
   state has been allocated after a snapshot has been taken, this piece of
   state does not need to be reset. *)

(* [register_ref r] registers a function that saves and restores the content
   of the reference [r]. *)
val register_ref: 'a ref -> unit
