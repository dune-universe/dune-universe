(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.md.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Lwt.Infix

module Bindings = Dlm_bindings.Bindings.Make(Generated)

let host = Errno_unix.host
let fail_errno ?(call="") ?(label="") errno =
  let open Errno in
  Errno_unix.to_unix_exn (Error {
      errno = of_code ~host errno;
      call; label;
    }) |> Lwt.fail

let check_opt ?(call="") ?(label="") v =
  v.Generated.lwt >>= function
  | None, errno ->
    (* when DLM module is not loaded we get NULL and errno set to 0 *)
    if errno = Signed.SInt.zero then
      Unix.Unix_error(Unix.ENOSYS, call, label) |> Lwt.fail
    else
      fail_errno ~call ~label errno
  | Some result, _ -> Lwt.return result

let check_int ?call ?label v =
  v.Generated.lwt >>= function
  | 0, _ -> Lwt.return_unit
  | -1, errno -> fail_errno ?call ?label errno
  | v, _ -> Lwt.fail_with (Printf.sprintf "Unknown return value %d" v)

open Bindings
type t = dlm_lshandle_t

let open_ name =
  check_opt ~call:"dlm_open_lockspace" ~label:name @@
  dlm_open_lockspace name >>= fun ls ->
  check_int ~call:"dlm_ls_pthread_init" ~label:name @@
  dlm_ls_pthread_init ls >>= fun () ->
  Lwt.return ls

let close ls =
  check_int ~call:"dlm_close_lockspace" @@ dlm_close_lockspace ls

let default_mode =
  PosixTypes.Mode.of_int 0o600

let create_destroy = Lwt_mutex.create ()

let join ?(mode=default_mode) name =
  Lwt.catch (fun () ->
      open_ name)
    (function
      | Unix.Unix_error _ ->
        Lwt_mutex.with_lock create_destroy (fun () ->
            check_opt ~call:"dlm_create_lockspace" ~label:name @@
            dlm_create_lockspace name mode)
      | e -> Lwt.fail e
    ) >>= close

let leave ?(force=false) name =
  let force_int = if force then 1 else 0 in
  open_ name >>= fun ls ->
  Lwt_mutex.with_lock create_destroy (fun () ->
      check_int ~call:"dlm_release_lockspace" ~label:name @@
      dlm_release_lockspace name ls force_int)

let with_lockspace name ~f =
  open_ name >>= fun ls ->
  Lwt.finalize (fun () -> f ls) (fun () -> close ls)

open Ctypes
open Dlm_bindings.Bindings.Types

let (|||) a b = Unsigned.UInt32.logor a b

type mode =
  | LKM_NLMODE
  | LKM_CRMODE
  | LKM_CWMODE
  | LKM_PRMODE
  | LKM_PWMODE
  | LKM_EXMODE

let mode_to_const = function
  | LKM_NLMODE -> Mode.lkm_nlmode
  | LKM_CRMODE -> Mode.lkm_crmode
  | LKM_CWMODE -> Mode.lkm_cwmode
  | LKM_PRMODE -> Mode.lkm_prmode
  | LKM_PWMODE -> Mode.lkm_pwmode
  | LKM_EXMODE -> Mode.lkm_exmode

let with_lock ls ?(mode=LKM_EXMODE) ?(try_=false) ?timeout name ~f =
  let open Dlm_lksb in
  let t = make t in
  setf t sb_status (-1);
  setf t sb_lkid Unsigned.UInt32.zero;
  setf t sb_flags '\x00';
  setf t sb_lvbptr '\x00';
  let do_lock mode flags timeout =
    let timeout_ptr = match timeout with
      | Some t ->
        Some (allocate uint64_t (t *. 100. |>
                                 Int64.of_float |>
                                 Unsigned.UInt64.of_int64))
      | None -> None
    in
    let t_orig = t in
    let t= addr t in
    check_int ~call:"dlm_ls_lockx" ~label:name @@
    Bindings.dlm_ls_lockx ls (mode_to_const mode) t flags
      name (String.length name |> Unsigned.UInt.of_int)
      Unsigned.UInt32.zero
      None
      None
      None
      None
      timeout_ptr >>= fun () ->
    let status = (getf t_orig sb_status) in
    if status <> 0 then
      fail_errno ~call:"dlm_ls_lockx.sb_status" ~label:name (Signed.SInt.of_int status)
    else
      Lwt.return_unit
  in
  do_lock LKM_NLMODE Flags.(lkf_expedite ||| lkf_wait) None >>= fun () ->
  Lwt.finalize (fun () ->
      let flags = Flags.(lkf_wait ||| lkf_convert) in
      let flags = if try_ then Flags.lkf_noqueue ||| flags else flags in
      let flags = if timeout = None then flags else Flags.(flags ||| lkf_timeout) in
      do_lock mode flags timeout >>= f)
    (fun () ->
       let lkid  = (getf t sb_lkid) in
       let t= addr t in
       let zero = Unsigned.UInt32.zero in
       check_int ~call:"dlm_ls_unlock_wait" ~label:name @@ Bindings.dlm_ls_unlock_wait ls lkid zero t)
