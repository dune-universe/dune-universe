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
let registeredtypeinfo : t structure typ = structure "Registered_type_info"

let get_type_name =
  foreign "g_registered_type_info_get_type_name"
    (ptr registeredtypeinfo @-> returning string_opt)

let get_g_type info =
  let get_g_type_raw =
    foreign "g_registered_type_info_get_g_type"
      (ptr registeredtypeinfo @-> returning Repository.gtype) in
  let gtype = get_g_type_raw info in
  if gtype == (Int64.shift_left (Int64.of_int 1) 2) then None (* G_TYPE_NONE *)
  else Some gtype

let get_type_init =
  foreign "g_registered_type_info_get_type_init"
    (ptr registeredtypeinfo @-> returning string_opt)

(* TODO : check that the info can be casted to vfunc info ? *)
let cast_from_baseinfo info =
  coerce (ptr Base_info.baseinfo) (ptr registeredtypeinfo) info

let cast_to_baseinfo info =
  coerce (ptr registeredtypeinfo) (ptr Base_info.baseinfo) info

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
