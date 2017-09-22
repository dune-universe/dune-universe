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

open Ctypes
module Types = Bindings_structs_lib.Bindings_structs.Make(Generated_types)
module Make(F: Cstubs.FOREIGN) = struct
  open F
  open PosixTypes

  external unix_error_of_errno : int -> Unix.error = "unix_error_of_code"
  type dlm_lshandle_t = unit ptr
  let dlm_lshandle_t : dlm_lshandle_t typ = ptr void
  let dlm_lshandle_t_opt : dlm_lshandle_t option typ = ptr_opt void

  (* always use [ptr_opt ...] to check for NULL return values,
     and take [ptr ...] as arguments to ensure non-NULL *)

  let dlm_create_lockspace = foreign "dlm_create_lockspace"
      (string @-> mode_t @-> returning dlm_lshandle_t_opt)
  (*  let dlm_new_lockspace = foreign "dlm_new_lockspace"
        (string @-> mode_t @-> uint32_t @-> returning dlm_lshandle_t)*)
  let dlm_open_lockspace = foreign "dlm_open_lockspace"
      (string @-> returning dlm_lshandle_t_opt)
  let dlm_ls_pthread_init = foreign "dlm_ls_pthread_init"
      (dlm_lshandle_t @-> returning int)

  let dlm_release_lockspace = foreign "dlm_release_lockspace"
      (string @-> dlm_lshandle_t @-> int @-> returning int)
  let dlm_close_lockspace = foreign "dlm_close_lockspace"
      (dlm_lshandle_t @-> returning int)

  let mode = uint32_t
  let flags = uint32_t
  let dlm_lksb = ptr Types.Dlm_lksb.t

  let ast_cb = ptr_opt void (* TODO: callback *)
  let bast_cb = ptr_opt void (* TODO: callback *)

  let dlm_ls_lockx = foreign "dlm_ls_lockx"
      (dlm_lshandle_t @-> mode @-> dlm_lksb @-> flags @-> string @-> uint @->
       uint32_t @-> ast_cb @-> ptr_opt void @-> bast_cb @->
       ptr_opt uint64_t @-> ptr_opt uint64_t @-> returning int)

  let dlm_ls_unlock_wait = foreign "dlm_ls_unlock_wait"
      (dlm_lshandle_t @-> uint32_t @-> uint32_t @-> dlm_lksb @-> returning int)
end
