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
open Ctypes
open GObject_introspection

let namespace = "GObject"
let typelib = Repository.require namespace ()
let const_name = "SIGNAL_FLAGS_MASK"

let get_constant_info () =
  match Repository.find_by_name namespace const_name with
  | None -> None
  | Some (base_info) ->
    match Base_info.get_type base_info with
    | Constant -> let const_info = Constant_info.from_baseinfo base_info in
      Some const_info
    | _ -> None

let constant_test fn =
  match get_constant_info () with
  | None -> assert_equal_string const_name "No base info found"
  | Some (info) -> fn info

let test_get_type _ =
  constant_test (fun info ->
      let type_info = Constant_info.get_type info in
      let str = Type_info.to_string type_info in
      assert_equal_string "unknown" str ;
      let tag = Type_info.get_tag type_info in
      assert_equal ~printer:(fun tag ->
          Bindings.Types.string_of_tag tag
        ) Bindings.Types.Int32 tag
    )

let test_get_value _ =
  constant_test (fun info ->
      let type_info = Constant_info.get_type info in
      match Type_info.get_tag type_info with
      | Bindings.Types.Int32 -> let argument = Constant_info.get_value info in
        let value = getf (!@argument) Types.v_int32 in
        assert_equal_int 511 (Int32.to_int value)
      | _ -> assert_equal_string "The tag should be " "Int32"
    )

let tests =
  "GObject Introspection ConstantInfo tests" >:::
  [
    "Constant_info get type" >:: test_get_type;
    "Constant_info get value" >:: test_get_value
  ]
