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
let fieldinfo : t structure typ = structure "Field_info"

let all_flags : (int64 * Bindings.Field_info.flags) list= [
    Stubs.Field_info.gi_field_is_readable, Is_readable;
    Stubs.Field_info.gi_field_is_writable, Is_writable;
  ]

let flags_list = Utils.generate_flags_list_view Stubs.Function_info.flags all_flags

let get_flags =
    foreign "g_field_info_get_flags"
      (ptr fieldinfo @-> returning flags_list)

let get_offset =
  foreign "g_field_info_get_offset"
    (ptr fieldinfo @-> returning int)

let get_size =
  foreign "g_field_info_get_size"
    (ptr fieldinfo @-> returning int)

let get_type info =
  let get_type_raw =
    foreign "g_field_info_get_type"
      (ptr fieldinfo @-> returning (ptr Type_info.typeinfo))
  in let info' = get_type_raw info in
  Type_info.add_unref_finaliser info'

(* TODO : check that the info can be casted to field info ? *)
let cast_from_baseinfo info =
  coerce (ptr Base_info.baseinfo) (ptr fieldinfo) info

let cast_to_baseinfo info =
  coerce (ptr fieldinfo) (ptr Base_info.baseinfo) info

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
