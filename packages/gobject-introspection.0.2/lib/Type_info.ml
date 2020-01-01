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
let typeinfo : t structure typ = structure "Type_info"

let to_string =
  foreign "g_info_type_to_string"
    (ptr typeinfo @-> returning string)

let is_pointer =
  foreign "g_type_info_is_pointer"
    (ptr typeinfo @-> returning bool)

let get_tag =
    foreign "g_type_info_get_tag"
      (ptr typeinfo @-> returning Stubs.Types.tag)

let get_array_length =
  foreign "g_type_info_get_array_length"
    (ptr typeinfo @-> returning int)

let get_array_fixed_size =
  foreign "g_type_info_get_array_fixed_size"
    (ptr typeinfo @-> returning int)

let is_zero_terminated =
  foreign "g_type_info_is_zero_terminated"
    (ptr typeinfo @-> returning bool)

let get_array_type info =
  let get_array_type_raw =
    foreign "g_type_info_get_array_type"
      (ptr typeinfo @-> returning Stubs.Types.array_type) in
  try Some (get_array_type_raw info) with
  | _ -> None

let get_interface info =
  let get_interface_raw =
    foreign "g_type_info_get_interface"
      (ptr typeinfo @-> returning (ptr_opt Base_info.baseinfo)) in
  match get_interface_raw info with
  | None -> None
  | Some info' -> let info'' = Base_info.add_unref_finaliser info' in
    Some info''

(* TODO : check that the info can be casted to arg info ? *)
let cast_from_baseinfo info =
  coerce (ptr Base_info.baseinfo) (ptr typeinfo) info

let cast_to_baseinfo info =
  coerce (ptr typeinfo) (ptr Base_info.baseinfo) info

let add_unref_finaliser info =
  let _ = Gc.finalise (fun i ->
      let i' = cast_to_baseinfo i in
      Base_info.base_info_unref i') info
  in info

let unsafe_get_param_type info n =
  let get_param_type_raw =
    foreign "g_type_info_get_param_type"
      (ptr typeinfo @-> int @-> returning (ptr typeinfo)) in
    let param_type = get_param_type_raw info n in
    add_unref_finaliser param_type

let get_param_type info =
  unsafe_get_param_type info 0

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
