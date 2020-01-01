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

open Test_utils
open OUnit2
open GObject_introspection

let namespace = "Gtk"
let typelib = Repository.require namespace ()
let object_name = "Window"
let vfunc_name = "activate_default"

let get_vfunc_info () =
  match Repository.find_by_name namespace object_name with
  | None -> None
  | Some (base_info) ->
    match Base_info.get_type base_info with
    | Object -> let object_info = Object_info.from_baseinfo base_info in
      Object_info.find_vfunc object_info vfunc_name
    | _ -> None

let vfunc_test fn =
  match get_vfunc_info () with
  | None -> assert_equal_string object_name "No base info found"
  | Some (info) -> fn info

let test_get_offset _ =
  vfunc_test (fun info ->
      let offset = VFunc_info.get_offset info in
      assert_equal_int 0xFFFF offset
    )

let test_get_signal _ =
  vfunc_test (fun info ->
      match VFunc_info.get_signal info with
      | None -> assert_equal_boolean true true
      | Some _ -> assert_equal_string "It should no return " "a callable info"
    )

let test_get_flags _ =
  vfunc_test (fun info ->
      let flags = VFunc_info.get_flags info in
      let rec check_flag = function
        | [] -> ()
        | flag :: remain ->let _ = assert_equal ~printer:(fun f ->
            Bindings.VFunc_info.string_of_flag f
          ) Bindings.VFunc_info.Must_override flag in
          check_flag remain
      in check_flag flags;
      assert_equal [] flags
    )

let tests =
  "GObject Introspection VFuncInfo tests" >:::
  [
    "VFunc_info get offset" >:: test_get_offset;
    "VFunc_info get signal" >:: test_get_signal;
    "VFunc_info get flags" >:: test_get_flags
  ]
