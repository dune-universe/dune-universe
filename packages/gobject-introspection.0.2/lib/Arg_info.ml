(*
 * Copyright 2017-2019 Cedric LE MOIGNE, cedlemo@gmx.com
 * This file is part of OCaml-GObject-Introspection.
 *
 * OCaml-GObject-Introspection is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * OCaml-GObject-Introspection is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with OCaml-GObject-Introspection.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Ctypes
open Foreign

type t
let arginfo : t structure typ = structure "Arg_info"

let get_direction =
    foreign "g_arg_info_get_direction"
      (ptr arginfo @-> returning Stubs.Arg_info.direction)

let get_closure =
  foreign "g_arg_info_get_closure"
    (ptr arginfo @-> returning int)

let get_destroy =
  foreign "g_arg_info_get_destroy"
    (ptr arginfo @-> returning int)

let get_ownership_transfer =
  foreign "g_arg_info_get_ownership_transfer"
    (ptr arginfo @-> returning Stubs.Arg_info.transfer)

let may_be_null =
  foreign "g_arg_info_may_be_null"
    (ptr arginfo @-> returning bool)

let is_caller_allocates =
  foreign "g_arg_info_is_caller_allocates"
    (ptr arginfo @-> returning bool)

let is_optional =
  foreign "g_arg_info_is_optional"
    (ptr arginfo @-> returning bool)

let is_return_value =
  foreign "g_arg_info_is_return_value"
    (ptr arginfo @-> returning bool)

let is_skip =
  foreign "g_arg_info_is_skip"
    (ptr arginfo @-> returning bool)

let get_scope =
    foreign "g_arg_info_get_scope"
      (ptr arginfo @-> returning Stubs.Arg_info.scope_type)

let get_type info =
  let get_type_raw =
    foreign "g_arg_info_get_type"
      (ptr arginfo @-> returning (ptr Type_info.typeinfo)) in
  let info' = get_type_raw info in
  Type_info.add_unref_finaliser info'

(* TODO : check that the info can be casted to arg info ? *)
let cast_from_baseinfo info =
  coerce (ptr Base_info.baseinfo) (ptr arginfo) info

let cast_to_baseinfo info =
  coerce (ptr arginfo) (ptr Base_info.baseinfo) info

let add_unref_finaliser info =
  let _ = Gc.finalise (fun i ->
      let i' = cast_to_baseinfo i in
      Base_info.base_info_unref i') info
  in info

let from_baseinfo info =
  let _ = Base_info.base_info_ref info in
  let info' = cast_from_baseinfo info in
  add_unref_finaliser info'

let to_baseinfo info =
  let info' = cast_to_baseinfo info in
  let _ = Base_info.base_info_ref info' in
  let _ = Gc.finalise (fun i ->
      Base_info.base_info_unref i) info' in
  info'
