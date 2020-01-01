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
let objectinfo : t structure typ = structure "Object_info"

let get_abstract =
  foreign "g_object_info_get_abstract"
    (ptr objectinfo @-> returning bool)

let get_fundamental =
  foreign "g_object_info_get_fundamental"
    (ptr objectinfo @-> returning bool)

let get_parent info =
  let get_parent_raw =
    foreign "g_object_info_get_parent"
      (ptr objectinfo @-> returning (ptr Base_info.baseinfo)) in
  let info' = get_parent_raw info in
  Base_info.add_unref_finaliser info'

let get_type_name =
  foreign "g_object_info_get_type_name"
    (ptr objectinfo @-> returning string)

let get_type_init =
  foreign "g_object_info_get_type_init"
    (ptr objectinfo @-> returning string)

let get_n_constants =
  foreign "g_object_info_get_n_constants"
    (ptr objectinfo @-> returning int)

let get_constant info n =
  let get_constant_raw =
    foreign "g_object_info_get_constant"
      (ptr objectinfo @-> int @-> returning (ptr Constant_info.constantinfo)) in
  let max = get_n_constants info in
  if (n < 0 || n >= max) then raise (Failure "Array Index out of bounds")
  else let info' = get_constant_raw info n in
    Constant_info.add_unref_finaliser info'

let get_n_fields =
  foreign "g_object_info_get_n_fields"
    (ptr objectinfo @-> returning int)

let get_field info n =
  let get_field_raw =
    foreign "g_object_info_get_field"
      (ptr objectinfo @-> int @-> returning (ptr Field_info.fieldinfo)) in
  let max = get_n_fields info in
  if (n < 0 || n >= max) then raise (Failure "Array Index out of bounds")
  else let info' = get_field_raw info n in
    Field_info.add_unref_finaliser info'

let get_n_interfaces =
  foreign "g_object_info_get_n_interfaces"
    (ptr objectinfo @-> returning int)

let get_interface info n =
  let get_interface_raw =
    foreign "g_object_info_get_interface"
      (ptr objectinfo @-> int @-> returning (ptr Interface_info.interfaceinfo)) in
  let max = get_n_interfaces info in
  if (n < 0 || n >= max) then raise (Failure "Array Index out of bounds")
  else let info' = get_interface_raw info n in
    Interface_info.add_unref_finaliser info'

let get_n_methods =
  foreign "g_object_info_get_n_methods"
    (ptr objectinfo @-> returning int)

let get_method info n =
  let get_method_raw =
    foreign "g_object_info_get_method"
      (ptr objectinfo @-> int @-> returning (ptr Function_info.functioninfo)) in
  let max = get_n_methods info in
  if (n < 0 || n >= max) then raise (Failure "Array Index out of bounds")
  else let info' = get_method_raw info n in
    Function_info.add_unref_finaliser info'

let find_method info name =
  let find_method_raw =
    foreign "g_object_info_find_method"
      (ptr objectinfo @-> string @-> returning (ptr_opt Function_info.functioninfo)) in
  match find_method_raw info name with
  | None -> None
  | Some info' -> let info'' = Function_info.add_unref_finaliser info' in
    Some info''

let get_n_properties =
  foreign "g_object_info_get_n_properties"
    (ptr objectinfo @-> returning int)

let get_property info n =
  let get_property_raw =
    foreign "g_object_info_get_property"
      (ptr objectinfo @-> int @-> returning (ptr Property_info.propertyinfo)) in
  let max = get_n_properties info in
  if (n < 0 || n >= max) then raise (Failure "Array Index out of bounds")
  else let info' = get_property_raw info n in
    Property_info.add_unref_finaliser info'

let get_n_signals =
  foreign "g_object_info_get_n_signals"
    (ptr objectinfo @-> returning int)

let get_signal info n =
  let get_signal_raw =
    foreign "g_object_info_get_signal"
      (ptr objectinfo @-> int @-> returning (ptr Signal_info.signalinfo)) in
  let max = get_n_signals info in
  if (n < 0 || n >= max) then raise (Failure "Array Index out of bounds")
  else let info' = get_signal_raw info n in
    Signal_info.add_unref_finaliser info'

let find_signal info name =
  let find_signal_raw =
    foreign "g_object_info_find_signal"
      (ptr objectinfo @-> string @-> returning (ptr_opt Signal_info.signalinfo)) in
  match find_signal_raw info name with
  | None -> None
  | Some info' -> let info'' = Signal_info.add_unref_finaliser info' in
    Some info''

let get_n_vfuncs =
  foreign "g_object_info_get_n_vfuncs"
    (ptr objectinfo @-> returning int)

let get_vfunc info n =
  let get_vfunc_raw =
    foreign "g_object_info_get_vfunc"
      (ptr objectinfo @-> int @-> returning (ptr VFunc_info.vfuncinfo)) in
  let max = get_n_vfuncs info in
  if (n < 0 || n >= max)  then raise (Failure "Array Index out of bounds")
  else let info' = get_vfunc_raw info n in
    VFunc_info.add_unref_finaliser info'

let find_vfunc info name =
  let find_vfunc_raw =
    foreign "g_object_info_find_vfunc"
      (ptr objectinfo @-> string @-> returning (ptr_opt VFunc_info.vfuncinfo)) in
  match find_vfunc_raw info name with
  | None -> None
  | Some info' -> let info'' = VFunc_info.add_unref_finaliser info' in
    Some info''

let get_class_struct info =
  let get_class_struct_raw =
    foreign "g_object_info_get_class_struct"
      (ptr objectinfo @-> returning (ptr_opt Struct_info.structinfo)) in
  match get_class_struct_raw info with
  | None -> None
  | Some info' -> let info'' = Struct_info.add_unref_finaliser info' in
    Some info''

let get_ref_function =
  foreign "g_object_info_get_ref_function"
    (ptr objectinfo @-> returning string_opt)

let get_unref_function =
  foreign "g_object_info_get_unref_function"
    (ptr objectinfo @-> returning string_opt)

let get_set_value_function =
  foreign "g_object_info_get_set_value_function"
    (ptr objectinfo @-> returning string_opt)

let get_get_value_function =
  foreign "g_object_info_get_get_value_function"
    (ptr objectinfo @-> returning string_opt)

(* TODO : check that the info can be casted to object info ? *)
let cast_from_baseinfo info =
  coerce (ptr Base_info.baseinfo) (ptr objectinfo) info

let cast_to_baseinfo info =
  coerce (ptr objectinfo) (ptr Base_info.baseinfo) info

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

let find_method_using_interfaces info name =
  let find_method_using_interfaces_raw =
    foreign "g_object_info_find_method_using_interfaces"
    (ptr objectinfo @-> string @-> ptr (ptr objectinfo) @->
     returning (ptr_opt Function_info.functioninfo)) in
  let implementor_addr = allocate_n (ptr objectinfo) ~count:1 in
  match find_method_using_interfaces_raw info name implementor_addr with
  | None -> (None, None)
  | Some info' -> (Some info',
     match coerce (ptr objectinfo) (ptr_opt objectinfo) (!@implementor_addr) with
    | None -> None
    | Some implementor -> Some (add_unref_finaliser implementor))

let find_vfunc_using_interfaces info name =
  let find_vfunc_using_interfaces_raw =
    foreign "g_object_info_find_vfunc_using_interfaces"
    (ptr objectinfo @-> string @-> ptr (ptr objectinfo) @->
     returning (ptr_opt VFunc_info.vfuncinfo)) in
  let implementor_addr = allocate_n (ptr objectinfo) ~count:1 in
  match find_vfunc_using_interfaces_raw info name implementor_addr with
  | None -> (None, None)
  | Some info' -> (Some info',
     match coerce (ptr objectinfo) (ptr_opt objectinfo) (!@implementor_addr) with
    | None -> None
    | Some implementor -> Some (add_unref_finaliser implementor))

(* TODO : check that the info can be casted to a objectinfo ? *)
let cast_from_registeredtypeinfo info =
  coerce (ptr Registered_type_info.registeredtypeinfo) (ptr objectinfo) info

let cast_to_registeredtypeinfo info =
  coerce (ptr objectinfo) (ptr Registered_type_info.registeredtypeinfo) info

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
