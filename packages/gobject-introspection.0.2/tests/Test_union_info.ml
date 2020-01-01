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

let namespace = "GLib"
let typelib = Repository.require namespace ()
let union_name = "Mutex"

let test_from_baseinfo _ =
  match Repository.find_by_name namespace union_name with
  | None -> assert_equal_string union_name "No base info found"
  | Some (base_info) -> assert_equal_boolean true (
      match Base_info.get_type base_info with
      | Union -> true
      | _ -> false )

let get_union_info () =
  match Repository.find_by_name namespace union_name with
  | None -> None
  | Some (base_info) ->
    match Base_info.get_type base_info with
    | Union -> let union_info = Union_info.from_baseinfo base_info in
      Some union_info
    | _ -> None

let union_test fn =
  match get_union_info () with
  | None -> assert_equal_string union_name "No base info found"
  | Some (info) -> fn info

let test_get_n_fields _ =
  union_test (fun info ->
    let n = Union_info.get_n_fields info in
    assert_equal_int 2 n
  )

let test_get_size _ =
  union_test (fun info ->
    let size = Union_info.get_size info in
    assert_equal_int 8 size
  )

let test_get_alignment _ =
  union_test (fun info ->
    let alignment = Union_info.get_alignment info in
    assert_equal_boolean true (alignment = 4 || alignment = 8)
  )

let test_get_n_methods _ =
  union_test (fun info ->
    let n = Union_info.get_n_methods info in
    assert_equal_int 5 n
  )

let test_get_field _ =
  union_test (fun info ->
    let field = Union_info.get_field info 0 in
      let flags = Field_info.get_flags field in
      let rec check_flags = function
        | [] -> ()
        | f' :: q -> let _ = assert_equal ~printer:(fun f ->
            match f with
            | Bindings.Field_info.Is_readable -> "readable"
            | Bindings.Field_info.Is_writable -> "writable"
          ) Bindings.Field_info.Is_readable f' in check_flags q
      in check_flags flags
    )

let test_get_field_out_of_bounds _ =
  union_test (fun info ->
    try ignore(Union_info.get_field info 3000)
    with
    | Failure message -> assert_equal_string "Array Index out of bounds"
                                              message
    | _ -> assert_equal_string "Bad exception" "Not a Failure"
  )

let test_get_method _ =
  union_test (fun info ->
    let m = Union_info.get_method info 0 in
    let symbol = Function_info.get_symbol m in
    assert_equal_string "g_mutex_clear" symbol
  )

let test_get_method_out_of_bounds _ =
  union_test (fun info ->
    try ignore(Union_info.get_method info 300)
    with
    | Failure message -> assert_equal_string "Array Index out of bounds"
                                              message
    | _ -> assert_equal_string "Bad exception" "Not a Failure"
  )

let test_find_method _ =
  let function_name = "clear" in
  union_test (fun info ->
      match Union_info.find_method info function_name with
      | None -> assert_equal_boolean true false
      | Some m -> let symbol = Function_info.get_symbol m in
        assert_equal_string ("g_mutex_" ^ function_name) symbol
    )

let test_is_discriminated _ =
  union_test (fun info ->
      let is_discriminated = Union_info.is_discriminated info in
      assert_equal_boolean false is_discriminated
  )

let tests =
  "GObject Introspection UnionInfo tests" >:::
  [
    "Union_info from baseinfo" >:: test_from_baseinfo;
    "Union_info get n fields" >:: test_get_n_fields;
    "Union_info get size" >:: test_get_size;
    "Union_info get alignment" >:: test_get_alignment;
    "Union_info get n methods" >:: test_get_n_methods;
    "Union_info get field" >:: test_get_field;
    "Union_info get field out of bounds" >:: test_get_field_out_of_bounds;
    "Union_info get method" >:: test_get_method;
    "Union_info get method out of bounds" >:: test_get_method_out_of_bounds;
    "Union_info find method" >:: test_find_method;
    "Union_info is discriminated" >:: test_is_discriminated
  ]
