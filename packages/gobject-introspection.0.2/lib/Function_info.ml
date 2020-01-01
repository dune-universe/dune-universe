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
let functioninfo : t structure typ = structure "Function_info"

let get_symbol =
  foreign "g_function_info_get_symbol"
    (ptr functioninfo @-> returning string)

let all_flags : (int64 * Bindings.Function_info.flags) list= [
    Stubs.Function_info.gi_function_is_method, Is_method;
    Stubs.Function_info.gi_function_is_constructor, Is_constructor;
    Stubs.Function_info.gi_function_is_getter, Is_getter;
    Stubs.Function_info.gi_function_is_setter, Is_setter;
    Stubs.Function_info.gi_function_wraps_vfunc, Wraps_vfunc;
    Stubs.Function_info.gi_function_throws, Throws;
  ]

let flags_list = Utils.generate_flags_list_view Stubs.Function_info.flags all_flags

let get_flags =
    foreign "g_function_info_get_flags"
      (ptr functioninfo @-> returning flags_list)

let get_property info =
  let flags = get_flags info in
  let rec find_set_get = function
    | [] -> false
    | h :: q -> match h with
      | Bindings.Function_info.Is_setter | Bindings.Function_info.Is_getter -> true
      | _ -> find_set_get q
  in if (find_set_get flags) then (
    let get_property_raw =
      foreign "g_function_info_get_property"
        (ptr functioninfo @-> returning (ptr_opt Property_info.propertyinfo)) in
    match get_property_raw info with
    | None -> None
    | Some info' -> let info'' = Property_info.add_unref_finaliser info' in
      Some info''
  )
  else None

let get_vfunc info =
  let flags = get_flags info in
  let rec has_wraps_vfunc = function
    | [] -> false
    | h :: q -> if h == Bindings.Function_info.Wraps_vfunc then true
      else has_wraps_vfunc q
  in
  if (has_wraps_vfunc flags) then
    let get_vfunc_raw =
      foreign "g_function_info_get_vfunc"
        (ptr functioninfo @-> returning (ptr_opt Callable_info.callableinfo)) in
    match get_vfunc_raw info with
    | None -> None
    | Some info' -> let info'' = Callable_info.add_unref_finaliser info' in
      Some info''
  else None

(* TODO : check that the info can be casted to function info ? *)
let cast_from_baseinfo info =
  coerce (ptr Base_info.baseinfo) (ptr functioninfo) info

let cast_to_baseinfo info =
  coerce (ptr functioninfo) (ptr Base_info.baseinfo) info

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

let cast_from_callableinfo info =
  coerce (ptr Callable_info.callableinfo) (ptr functioninfo) info

let cast_to_callableinfo info =
  coerce (ptr functioninfo) (ptr Callable_info.callableinfo) info

let to_callableinfo info =
  let info' = cast_to_baseinfo info in
  let _ = Base_info.base_info_ref info' in
  let info'' = cast_to_callableinfo info in
  Callable_info.add_unref_finaliser info''

let from_callableinfo info =
  let info' = Callable_info.cast_to_baseinfo info in
  let _ = Base_info.base_info_ref info' in
  let info'' = cast_from_callableinfo info in
  let _ = Gc.finalise (fun i ->
      let i' = cast_to_baseinfo i in
      Base_info.base_info_unref i') info'' in
  info''
