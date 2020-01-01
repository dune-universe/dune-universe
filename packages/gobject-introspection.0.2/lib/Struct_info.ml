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
let structinfo : t structure typ = structure "Struct_info"

let is_gtype_struct =
  foreign "g_struct_info_is_gtype_struct"
    (ptr structinfo @-> returning bool)

let get_alignment =
  foreign "g_struct_info_get_alignment"
    (ptr structinfo @-> returning int)

let get_size =
  foreign "g_struct_info_get_size"
    (ptr structinfo @-> returning int)

let is_foreign =
  foreign "g_struct_info_is_foreign"
    (ptr structinfo @-> returning bool)

let get_n_fields =
  foreign "g_struct_info_get_n_fields"
    (ptr structinfo @-> returning int)

let get_n_methods =
  foreign "g_struct_info_get_n_methods"
    (ptr structinfo @-> returning int)

let get_field info n =
  let get_field_raw =
    foreign "g_struct_info_get_field"
      (ptr structinfo @-> int @-> returning (ptr Field_info.fieldinfo)) in
  let max = get_n_fields info in
  if (n < 0 || n >= max) then raise (Failure "Array Index out of bounds")
  else let info' = get_field_raw info n in
    Field_info.add_unref_finaliser info'

let get_method info n =
  let get_method_raw =
    foreign "g_struct_info_get_method"
      (ptr structinfo @-> int @-> returning (ptr Function_info.functioninfo)) in
  let max = get_n_methods info in
  if (n < 0 || n >= max) then raise (Failure "Array Index out of bounds")
  else let info' = get_method_raw info n in
    Function_info.add_unref_finaliser info'

let find_method info name =
  let find_method_raw =
    foreign "g_struct_info_find_method"
    (ptr structinfo @-> string @-> returning (ptr_opt Function_info.functioninfo))
  in match find_method_raw info name with
  | None -> None
  | Some info' ->
    let fn_info = Function_info.add_unref_finaliser info' in
    Some fn_info

(* TODO : check that the info can be casted to a structinfo ? *)
let cast_from_baseinfo info =
  coerce (ptr Base_info.baseinfo) (ptr structinfo) info

let cast_to_baseinfo info =
  coerce (ptr structinfo) (ptr Base_info.baseinfo) info

let add_unref_finaliser info =
  let _ = Gc.finalise (fun i ->
      let i' = cast_to_baseinfo i in
      Base_info.base_info_unref i') info in
  info

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

(* TODO : check that the info can be casted to a structinfo ? *)
let cast_from_registeredtypeinfo info =
  coerce (ptr Registered_type_info.registeredtypeinfo) (ptr structinfo) info

let cast_to_registeredtypeinfo info =
  coerce (ptr structinfo) (ptr Registered_type_info.registeredtypeinfo) info

let from_registeredtypeinfo info =
  let base_info = Registered_type_info.cast_to_baseinfo info in
  let _ = Base_info.base_info_ref base_info in
  let info' = cast_from_registeredtypeinfo info in
  let _ = Gc.finalise (fun i ->
      let i' = cast_to_baseinfo i in
      Base_info.base_info_unref i') info' in
  info'

let to_registeredtypeinfo info =
  let base_info = cast_to_baseinfo info in
  let _ = Base_info.base_info_ref base_info in
  let info' = cast_to_registeredtypeinfo info in
  let _ = Gc.finalise (fun i ->
      let i' = Registered_type_info.cast_to_baseinfo i in
      Base_info.base_info_unref i') info' in
  info'
