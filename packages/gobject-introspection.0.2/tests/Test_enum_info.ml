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

let namespace = "Gio"
let typelib = Repository.require namespace ()
let enum_name = "ResourceError"

let get_enum_info () =
  match Repository.find_by_name namespace enum_name with
  | None -> None
  | Some (base_info) ->
    match Base_info.get_type base_info with
    | Enum -> let info = Enum_info.from_baseinfo base_info
      in Some info
    | _ -> None

let enum_test fn =
  match get_enum_info () with
  | None -> assert_equal_string enum_name "No base info found"
  | Some (info) -> fn info

let test_get_n_values _ =
  enum_test (fun info ->
      let n_values = Enum_info.get_n_values info in
      assert_equal_int 2 n_values
    )

let test_get_n_methods _ =
  enum_test (fun info ->
      let n_methods = Enum_info.get_n_methods info in
      assert_equal_int 1 n_methods
    )

let test_get_method _ =
  enum_test (fun info ->
      let m = Enum_info.get_method info 0 in
      let name = Function_info.get_symbol m in
      assert_equal_string "g_resource_error_quark" name
    )

let test_get_value _ =
  enum_test (fun info ->
      match Enum_info.get_value info 1 with
      | None -> assert_equal_string "No value " "found"
      | Some value -> let value' = Value_info.get_value value in
        let _ = assert_equal 1 (Int64.to_int value') in
        let base = Value_info.to_baseinfo value in
        match Base_info.get_name base with
        | None -> ()
        | Some name -> assert_equal_string "internal" name
    )

let test_get_error_domain _ =
  enum_test (fun info ->
      match Enum_info.get_error_domain info with
      | None -> assert_equal_boolean true false
      | Some error_domain -> assert_equal_string "g-resource-error-quark" error_domain
    )

let test_get_storage_type _ =
  enum_test (fun info ->
      match Enum_info.get_storage_type info with
      | Bindings.Types.Uint32 -> assert_equal true true
      | _ -> assert_equal_string "bad " "type"
    )

let tests =
  "GObject Introspection Enum_info tests" >:::
  [
    "Enum_info get n values" >:: test_get_n_values;
    "Enum_info get n methods" >:: test_get_n_methods;
    "Enum_info get method" >:: test_get_method;
    "Enum_info get value" >:: test_get_value;
    "Enum_info get error domain" >:: test_get_error_domain;
    "Enum_info get storage type" >:: test_get_storage_type
  ]
