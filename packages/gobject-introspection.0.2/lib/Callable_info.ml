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

(** Function_info — Struct representing a function. *)

open Ctypes
open Foreign

type t
let callableinfo : t structure typ = structure "Callable_info"

let can_throw_gerror =
  foreign "g_callable_info_can_throw_gerror"
    (ptr callableinfo @-> returning bool)

let get_n_args =
  foreign "g_callable_info_get_n_args"
    (ptr callableinfo @-> returning int)

let get_return_attribute =
  foreign "g_callable_info_get_return_attribute"
    (ptr callableinfo @-> returning string_opt)

let is_method =
  foreign "g_callable_info_is_method"
    (ptr callableinfo @-> returning bool)

let may_return_null =
  foreign "g_callable_info_may_return_null"
    (ptr callableinfo @-> returning bool)

let skip_return =
  foreign "g_callable_info_skip_return"
    (ptr callableinfo @-> returning bool)

let get_arg info n =
  let get_arg_raw =
    foreign "g_callable_info_get_arg"
      (ptr callableinfo @-> int @-> returning (ptr Arg_info.arginfo))
  in let max = get_n_args info in
  if (n < 0 || n >= max) then raise (Failure "Array Index out of bounds")
  else
    let info' = get_arg_raw info n in
    Arg_info.add_unref_finaliser info'

let get_return_type info =
  let get_return_type_raw =
    foreign "g_callable_info_get_return_type"
      (ptr callableinfo @-> returning (ptr Type_info.typeinfo))
  in let info' = get_return_type_raw info in
  Type_info.add_unref_finaliser info'

let get_caller_owns =
  foreign "g_callable_info_get_caller_owns"
    (ptr callableinfo @-> returning Stubs.Arg_info.transfer)

let cast_from_baseinfo info =
  coerce (ptr Base_info.baseinfo) (ptr callableinfo) info

let cast_to_baseinfo info =
  coerce (ptr callableinfo) (ptr Base_info.baseinfo) info

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
