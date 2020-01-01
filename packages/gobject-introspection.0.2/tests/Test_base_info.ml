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
let info_name = "app_info_create_from_commandline"
let callback_name = "BusNameAppearedCallback"

let test_equal _ =
  let rand_info = Repository.get_info namespace 10 in
  match Base_info.get_name rand_info with
  | None -> assert_equal_string "Base Info " "has no name"
  | Some info_name ->
    match Repository.find_by_name namespace info_name with
    | None -> assert_equal_string info_name "No base info found"
    | Some (info) -> let is_equal = Base_info.equal rand_info info in
      assert_equal_boolean true is_equal

let test_get_namespace _ =
  match Repository.find_by_name namespace info_name with
  | None -> assert_equal_string info_name "No base info found"
  | Some (base_info) -> let info_namespace = Base_info.get_namespace base_info
  in assert_equal_string namespace info_namespace

let test_is_deprecated _ =
  match Repository.find_by_name  namespace info_name with
  | None -> assert_equal_string info_name "No base info found"
  | Some (base_info) -> let is_deprecated = Base_info.is_deprecated base_info in
    assert_equal_boolean false is_deprecated

let test_get_container _ =
  match Repository.find_by_name  namespace info_name with
  | None -> assert_equal_string info_name "No base info found"
  | Some base_info -> (
      match Base_info.get_container base_info with
    | None -> assert_equal_boolean true true
    | Some _ -> assert_equal_string "It should not " "return a container here"
  )

let tests =
  "GObject Introspection BaseInfo tests" >:::
    [
      "Base_info equal" >:: test_equal;
      "Base_info get namespace" >:: test_get_namespace;
      "Base_info is deprecated" >:: test_is_deprecated;
      "Base_info get_container" >:: test_get_container
    ]
