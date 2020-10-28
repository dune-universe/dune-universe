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

(* Before [save()] has been called, we maintain a list of [save] functions that
   have been registered with us. When [save()] is called, we apply each of them
   to one argument, so we obtain a list of [reset] functions, which we store.
   When [reset()] is called, we invoke each of these functions. *)

type status =
  | BeforeSaving of (unit -> unit -> unit) list
  | AfterSaving of (unit -> unit) list

let status =
  ref (BeforeSaving [])

let register save =
  match !status with
  | BeforeSaving saves ->
      status := BeforeSaving (save :: saves)
  | AfterSaving _ ->
      (* [register] after [save] has no effect. *)
      ()

let call f =
  f()

let save () =
  match !status with
  | BeforeSaving saves ->
      status := AfterSaving (List.map call saves)
  | AfterSaving _ ->
      assert false (* protocol violation *)

let reset () =
  match !status with
  | BeforeSaving _ ->
      assert false (* protocol violation *)
  | AfterSaving resets ->
      List.iter call resets

let register_ref r =
  register (fun () ->
    let snapshot = !r in
    fun () ->
      r := snapshot
  )
