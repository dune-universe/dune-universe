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
let constantinfo: t structure typ = structure "Constant_info"

let get_type info =
  let get_type_raw =
    foreign "g_constant_info_get_type"
      (ptr constantinfo @-> returning (ptr Type_info.typeinfo)) in
  let info' = get_type_raw info in
  Type_info.add_unref_finaliser info'

(* TODO : check that the info can be casted to a constantinfo ? *)
let cast_from_baseinfo info =
  coerce (ptr Base_info.baseinfo) (ptr constantinfo) info

let cast_to_baseinfo info =
  coerce (ptr constantinfo) (ptr Base_info.baseinfo) info

let add_unref_finaliser info =
  let _ = Gc.finalise (fun i ->
      let i' = cast_to_baseinfo i in
      Base_info.base_info_unref i') info in
  info

let from_baseinfo info =
  let _ = Base_info.base_info_ref info in
  let info' = cast_from_baseinfo info in
  let _ = Gc.finalise (fun i ->
      let i' = cast_to_baseinfo i in
      Base_info.base_info_unref i') info' in
  info'

let to_baseinfo info =
  let info' = cast_to_baseinfo info in
  let _ = Base_info.base_info_ref info' in
  let _ = Gc.finalise (fun i ->
      Base_info.base_info_unref i) info' in
  info'

let info_free_value =
  foreign "g_constant_info_free_value"
    (ptr constantinfo @-> ptr Types.argument @-> returning void)

let get_value info =
  let get_value_raw =
    foreign "g_constant_info_get_value"
      (ptr constantinfo @-> ptr Types.argument @-> returning int) in
  let arg_ptr = allocate_n Types.argument ~count:1 in
  let _ = get_value_raw info arg_ptr in
  let _ = Gc.finalise (fun v ->
      info_free_value info v) arg_ptr in
  arg_ptr
