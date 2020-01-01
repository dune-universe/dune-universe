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

let get_function_info namespace name =
  match Repository.find_by_name  namespace name with
  | None -> None
  | Some (base_info) -> match Base_info.get_type base_info with
    | Function -> let info = Function_info.from_baseinfo base_info in
      Some info
    | _ -> None

let test_function_info namespace name fn =
  match get_function_info namespace name with
  | None -> assert_equal_string name "No base info found"
  | Some info -> fn info

let namespace = "GObject"
let typelib = Repository.require namespace ()
let name = "signal_name"

let test_get_symbol _ =
  test_function_info namespace name (fun info ->
      assert_equal "g_signal_name" (Function_info.get_symbol info)
    )

let test_get_property _ =
  test_function_info namespace name (fun info ->
      match Function_info.get_property info with
      | None -> assert_equal true true
      | Some _ -> assert_equal_string "It should not " "returns something"
    )

let test_get_vfunc _ =
  test_function_info namespace name (fun info ->
      match Function_info.get_vfunc info with
      | None -> assert_equal true true
      | Some _ -> assert_equal_string "It should not " "returns something"
    )

let test_get_empty_flags _ =
  test_function_info namespace name (fun info ->
      assert_equal [] (Function_info.get_flags info)
    )

let namespace = "Gio"
let typelib = Repository.require namespace ()
let name = "File"

let test_get_flags _ =
  match Repository.find_by_name namespace name with
  | None -> assert_equal_string "It should " "returns an object for Gio::File"
  | Some (base_info) -> match Base_info.get_type base_info with
    | Interface -> begin
        let info = Interface_info.from_baseinfo base_info in
        match Interface_info.find_method info "set_attribute" with
        | Some info' ->
          let flags = Function_info.get_flags info' in
          let len = List.length flags in
          assert_equal_int 2 len;
          assert_bool "It is a method" (List.mem Bindings.Function_info.Is_method flags);
          assert_bool "Throws" (List.mem Bindings.Function_info.Throws flags)
        | None -> assert_equal_string "Gio::File#set_attribute"  "should exists"
      end
    | baseinfo_t ->
      assert_equal_string "Gio::File should be an Interface instead of " (Bindings.Base_info.string_of_info_type baseinfo_t)

let tests =
  "GObject Introspection FunctionInfo tests" >:::
  [
    "Function_info get symbol" >:: test_get_symbol;
    "Function_info get property" >:: test_get_property;
    "Function_info get vfunc" >:: test_get_vfunc;
    "Function_info get empty flags" >:: test_get_empty_flags;
    "Function_info get flags" >:: test_get_flags;
  ]
