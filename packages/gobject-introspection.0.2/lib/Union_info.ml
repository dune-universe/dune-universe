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
let unioninfo : t structure typ = structure "Union_info"

let get_n_fields =
  foreign "g_union_info_get_n_fields"
    (ptr unioninfo @-> returning int)

let get_size =
  foreign "g_union_info_get_size"
    (ptr unioninfo @-> returning int)

let get_alignment =
  foreign "g_union_info_get_alignment"
    (ptr unioninfo @-> returning int)

let get_n_methods =
  foreign "g_union_info_get_n_methods"
    (ptr unioninfo @-> returning int)

let get_field info n =
  let get_field_raw =
    foreign "g_union_info_get_field"
      (ptr unioninfo @-> int @-> returning (ptr Field_info.fieldinfo)) in
  let max = get_n_fields info in
  if (n < 0 || n >= max) then raise (Failure "Array Index out of bounds")
  else let info' = get_field_raw info n in
    Field_info.add_unref_finaliser info'

let get_method info n =
  let get_method_raw =
    foreign "g_union_info_get_method"
      (ptr unioninfo @-> int @-> returning (ptr Function_info.functioninfo)) in
  let max = get_n_methods info in
  if (n < 0 || n >= max) then raise (Failure "Array Index out of bounds")
  else let info' = get_method_raw info n in
    Function_info.add_unref_finaliser info'

let find_method info name =
  let find_method_raw =
    foreign "g_union_info_find_method"
    (ptr unioninfo @-> string @-> returning (ptr_opt Function_info.functioninfo))
  in match find_method_raw info name with
  | None -> None
  | Some info' ->
    let fn_info = Function_info.add_unref_finaliser info' in
    Some fn_info

let is_discriminated =
  foreign "g_union_info_is_discriminated"
    (ptr unioninfo @-> returning bool)

(* TODO : get_discriminator_offset find a test value *)
let get_discriminator_offset =
  foreign "g_union_info_get_discriminator_offset"
    (ptr unioninfo @-> returning int)

(* TODO : get_discriminator_type find a test value *)
let get_discriminator_type info =
  let get_discriminator_type_raw =
    foreign "g_union_info_get_discriminator_type"
      (ptr unioninfo @-> returning (ptr Type_info.typeinfo)) in
  let info' = get_discriminator_type_raw info in
  Type_info.add_unref_finaliser info'

(* TODO : get_discriminator find a test  *)
let get_discriminator info n =
  let get_discriminator_raw =
    foreign "g_union_info_get_discriminator"
    (ptr unioninfo @-> int @-> returning (ptr Constant_info.constantinfo)) in
  let info' = get_discriminator_raw info n in
  Constant_info.add_unref_finaliser info'

(* TODO : check that the info can be casted to a unioninfo ? *)
let cast_from_baseinfo info =
  coerce (ptr Base_info.baseinfo) (ptr unioninfo) info

let cast_to_baseinfo info =
  coerce (ptr unioninfo) (ptr Base_info.baseinfo) info

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

(* TODO : check that the info can be casted to a unioninfo ? *)
let cast_from_registeredtypeinfo info =
  coerce (ptr Registered_type_info.registeredtypeinfo) (ptr unioninfo) info

let cast_to_registeredtypeinfo info =
  coerce (ptr unioninfo) (ptr Registered_type_info.registeredtypeinfo) info

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
