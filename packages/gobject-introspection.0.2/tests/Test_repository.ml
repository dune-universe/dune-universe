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

open OUnit2
open Test_utils
open GObject_introspection

let test_get_default _ =
  let _ = Repository.get_default () in
  assert_equal_boolean true true

let test_require _ =
  let repository = Repository.get_default () in
  let namespace = "Gio" in
  match Repository.require ~repository namespace () with
  | Error message -> assert_equal_string "Unable to load namespace Gio" message
  | Ok _ -> assert_equal_boolean true true

let test_require_fail _ =
  let repository = Repository.get_default () in
  let namespace = "bad_namespace" in
  match Repository.require ~repository namespace () with
  | Error message -> assert_equal_string "Unable to load namespace bad_namespace" message
  | Ok _ -> assert_equal_boolean true true

let namespace = "Gio"
let repository = Repository.get_default ()
let typelib = Repository.require ~repository namespace ()

let test_get_loaded_namespaces _ =
  let namespaces_check = "GLib GObject Gio" in
  let namespaces = String.concat " " (Repository.get_loaded_namespaces ~repository ()) in
  assert_equal_string namespaces_check namespaces

let test_get_dependencies _ =
  let dependencies_check = "GLib-2.0 GObject-2.0" in
  let dependencies = String.concat " " (Repository.get_dependencies ~repository namespace) in
  assert_equal_string dependencies_check dependencies

let test_get_c_prefix _ =
  let c_prefix = Repository.get_c_prefix ~repository namespace in
  assert_equal_string "G" c_prefix

let test_get_version _ =
  let version = Repository.get_version ~repository namespace in
  assert_equal_string "2.0" version

let test_get_typelib_path _ =
  let path = Repository.get_typelib_path ~repository namespace in
  assert_equal_string "/usr/lib/girepository-1.0/Gio-2.0.typelib" path

let test_enumerate_versions _ =
  let versions_check = "2.0 2.0" in
  let versions = String.concat " " (Repository.enumerate_versions ~repository namespace) in
  assert_equal_string versions_check versions

let test_get_search_path _ =
  let path = String.concat " " (Repository.get_search_path ()) in
  assert_equal_string "/usr/lib/girepository-1.0" path

let test_prepend_search_path _ =
  let initial_path = String.concat " " (Repository.get_search_path ()) in
  let new_path = "/home/myhome" in
  let _ = Repository.prepend_search_path new_path in
  let paths = String.concat " " (Repository.get_search_path ()) in
  let initial_paths = String.concat " " [new_path; initial_path] in
  assert_equal_string initial_paths paths

let test_find_by_name _ =
  let info_name = "Application" in
  match Repository.find_by_name ~repository namespace info_name with
  | None -> assert_equal_string info_name "No base info found"
  | Some (base_info) -> match Base_info.get_name base_info with
    | None -> assert_equal_string info_name "No name found"
    | Some name -> assert_equal_string info_name name

let test_get_n_infos _ =
  let n_infos = Repository.get_n_infos ~repository namespace in
  assert_equal_int 702 n_infos

let test_get_info_out_of_bounds _ =
  try ignore (Repository.get_info ~repository namespace 1500)
  with
  | Failure message -> assert_equal_string "Array Index out of bounds"
                                              message
  | _ -> assert_equal_string "Bad exception" "Not a Failure"

let test_get_info _ =
  let info_name = "Action" in
  let info = Repository.get_info ~repository namespace 0 in
  match Base_info.get_name info with
  | None -> assert_equal_string info_name "No name found"
  | Some name -> assert_equal_string info_name name

let test_get_shared_library _ =
  match Repository.get_shared_library ~repository namespace with
  | None -> assert_equal_string "It should return " "something"
  | Some shared_lib -> assert_includes_string shared_lib "libgio-2.0"

let tests =
  "GObject Introspection Repository tests" >:::
    [
      "Repository get default" >:: test_get_default;
      "Repository require" >:: test_require;
      "Repository require fail" >:: test_require_fail;
      (* Disable because there is only one instance of Repository and those
       * namespaces depends on the nampespaces loaded previously and can
       * interfers with previous test.
       * "Repository get loaded namespaces" >:: test_get_loaded_namespaces;*)
      "Repository get c prefix" >:: test_get_c_prefix;
      "Repository get version" >:: test_get_version;
      (* Tests depend too much on system (ubuntu, arch, centos ...)
        "Repository get typelib path" >:: test_get_typelib_path;
        "Repository prepend search path" >:: test_prepend_search_path;
        "Repository get search path" >:: test_get_search_path;
        "Repository get dependencies" >:: test_get_dependencies;
        "Repository get n infos" >:: test_get_n_infos *)
      "Repository enumerate versions" >:: test_enumerate_versions;
      "Repository find by name" >:: test_find_by_name;
      "Repository get info out of bounds" >:: test_get_info_out_of_bounds;
      "Repository get info" >:: test_get_info;
      "Repository get shared library" >:: test_get_shared_library
    ]
