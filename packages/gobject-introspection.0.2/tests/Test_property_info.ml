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
let property_name = "accept-focus"

let test_get_property_from_repo _ =
  match Repository.find_by_name namespace "Window" with
  | None -> assert_equal_string property_name "No base info found"
  | Some (base_info) ->
    match Base_info.get_type base_info with
    | Object -> (
        let info = Object_info.from_baseinfo base_info in
        let prop = Object_info.get_property info 0 in
        let base_prop = Property_info.to_baseinfo prop in
        match  Base_info.get_type base_prop with
        | Property -> assert_equal true true
        | _ -> assert_equal_string "It should be a" "Property info"
      )
    | _ -> assert_equal_string property_name "No base info found"

let get_property_info () =
  match Repository.find_by_name namespace "Window" with
  | None -> None
  | Some (base_info) ->
    match Base_info.get_type base_info with
    | Object -> let info = Object_info.from_baseinfo base_info in
        let prop = Object_info.get_property info 0 in
        Some prop
    | _ -> None

let property_test fn =
  match get_property_info () with
  | None -> assert_equal_string property_name "No base info found"
  | Some (info) -> fn info

let test_get_ownership_transfer _ =
  property_test (fun info ->
      let transfer = Property_info.get_ownership_transfer info in
      assert_equal ~printer:(fun t ->
          match t with
          | Bindings.Arg_info.Nothing -> "nothing"
          | Bindings.Arg_info.Container -> "container"
          | Bindings.Arg_info.Everything -> "everything"
        ) Bindings.Arg_info.Nothing transfer
    )

let test_get_type _ =
  property_test (fun info ->
      let info = Property_info.get_type info in
      let type_name = Type_info.to_string info in
      assert_equal_string "unknown" type_name
    )

let test_get_flags _ =
  property_test (fun info ->
      let flags = Property_info.get_flags info in
      let n = List.length flags in
      let _ = assert_equal ~printer:string_of_int 3 n in
      let flag_names = [
        Bindings.GParam.Readwrite;
        Bindings.GParam.Writable;
        Bindings.GParam.Readable] in
      let _ = List.fold_left (fun acc flag ->
          let _ = assert_equal ~printer:(fun f -> GParam.flag_to_string f) (List.nth flag_names acc) flag in
          acc + 1
        ) 0 flags
      in ()
    )

let tests =
  "GObject Introspection InterfaceInfo tests" >:::
  [
    "Property_info find from repo" >:: test_get_property_from_repo;
    "Property_info get ownership transfer" >:: test_get_ownership_transfer;
    "Property_info get type" >:: test_get_type;
    "Property_info get flags" >:: test_get_flags
  ]
