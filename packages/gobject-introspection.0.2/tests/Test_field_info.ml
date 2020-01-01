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

let get_union_info () =
  match Repository.find_by_name namespace union_name with
  | None -> None
  | Some (base_info) ->
    match Base_info.get_type base_info with
    | Union -> let info = Union_info.from_baseinfo base_info
      in Some info
    | _ -> None

let field_test fn =
  match get_union_info () with
  | None -> assert_equal_string union_name "No base info found"
  | Some (info) -> fn (Union_info.get_field info 0)

let test_get_flags _ =
  field_test (fun info ->
      let flags = Field_info.get_flags info in
      let rec check_flags = function
        | [] -> ()
        | f' :: q -> let _ = assert_equal ~printer:(fun f ->
            match f with
            | Bindings.Field_info.Is_readable -> "readable"
            | Bindings.Field_info.Is_writable -> "writable"
          ) Bindings.Field_info.Is_readable f' in check_flags q
      in check_flags flags
    )

let test_get_offset _ =
  field_test (fun info ->
      let offset = Field_info.get_offset info in
      assert_equal_int 0 offset
    )

let test_get_size _ =
  field_test (fun info ->
      let size = Field_info.get_size info in
      assert_equal_int 0 size
  )

let test_get_type _ =
  field_test (fun info ->
      let info_type = Field_info.get_type info in
      let type_name = Type_info.to_string info_type in
      assert_equal_string "unknown" type_name
    )

let tests =
  "GObject Introspection FieldInfo tests" >:::
  [
    "Field_info get flags" >:: test_get_flags;
    "Field_info get offset" >:: test_get_offset;
    "Field_info get size" >:: test_get_size;
    "Field_info get type" >:: test_get_type
  ]
