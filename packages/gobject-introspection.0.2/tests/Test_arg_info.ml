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

let namespace = "GObject"
let typelib = Repository.require namespace ()
let func_name = "signal_name"

let get_arg_info () =
  match Repository.find_by_name namespace func_name with
  | None -> None
  | Some (base_info) ->
    match Base_info.get_type base_info with
    | Function -> let callable_info = Callable_info.from_baseinfo base_info
      in let info = Callable_info.get_arg callable_info 0 in Some info
    | _ -> None

let arg_test fn =
  match get_arg_info () with
  | None -> assert_equal_string func_name "No base info found"
  | Some (info) -> fn info

let test_get_direction _ =
  arg_test (fun info ->
      let dir = Arg_info.get_direction info in
      assert_equal ~printer:(fun d ->
          match d with
          | Bindings.Arg_info.In -> "In"
          | Bindings.Arg_info.Out -> "Out"
          | Bindings.Arg_info.InOut -> "InOut") Bindings.Arg_info.In dir
    )

let test_get_closure _ =
  arg_test (fun info ->
      let closure = Arg_info.get_closure info in
      assert_equal_int (-1) closure
    )

let test_get_destroy _ =
  arg_test (fun info ->
      let index = Arg_info.get_destroy info in
      assert_equal_int (-1) index
    )

let test_get_ownership_transfer _ =
  arg_test (fun info ->
      let transfer = Arg_info.get_ownership_transfer info in
      assert_equal ~printer:(fun t ->
          match t with
          | Bindings.Arg_info.Nothing -> "nothing"
          | Bindings.Arg_info.Container -> "container"
          | Bindings.Arg_info.Everything -> "everything"
        ) Bindings.Arg_info.Nothing transfer
    )

let test_may_be_null _ =
  arg_test (fun info ->
      let may_be_null = Arg_info.may_be_null info in
      assert_equal_boolean false may_be_null
    )

let test_is_caller_allocates _ =
  arg_test (fun info ->
      let caller_allocates = Arg_info.is_caller_allocates info in
      assert_equal_boolean false caller_allocates
    )

let test_is_optional _ =
  arg_test (fun info ->
      let is_optional = Arg_info.is_optional info in
      assert_equal_boolean false is_optional
    )

let test_is_return_value _ =
  arg_test (fun info ->
      let is_return = Arg_info.is_return_value info in
      assert_equal_boolean false is_return
    )

let test_is_skip _ =
  arg_test (fun info ->
      let is_skip = Arg_info.is_skip info in
      assert_equal_boolean false is_skip
    )

let test_get_scope _ =
  arg_test (fun info ->
      let scope = Arg_info.get_scope info in
      assert_equal ~printer:(fun scope ->
          match scope with
          | Bindings.Arg_info.Invalid -> "Invalid"
          | Bindings.Arg_info.Call -> "Call"
          | Bindings.Arg_info.Async -> "Async"
          | Bindings.Arg_info.Notified -> "Notified"
        ) Bindings.Arg_info.Invalid scope
    )

let test_get_type _ =
  arg_test (fun info ->
        let type_info = Arg_info.get_type info in
        let type_name = Type_info.to_string type_info in
        assert_equal_string "unknown" type_name
      )

let tests =
  "GObject Introspection ArgInfo tests" >:::
  [
    "Arg_info get direction" >:: test_get_direction;
    "Arg_info get closure" >:: test_get_closure;
    "Arg_info get destroy" >:: test_get_destroy;
    "Arg_info get ownership transfer" >:: test_get_ownership_transfer;
    "Arg_info may be null" >:: test_may_be_null;
    "Arg_info is caller allocates" >:: test_is_caller_allocates;
    "Arg_info is optional" >:: test_is_optional;
    "Arg_info is return value" >:: test_is_return_value;
    "Arg_info is skip" >:: test_is_skip;
    "Arg_info get scope" >:: test_get_scope;
    "Arg_info get type" >:: test_get_type
  ]
