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

let get_callable_info () =
  match Repository.find_by_name namespace func_name with
  | None -> None
  | Some (base_info) ->
    match Base_info.get_type base_info with
    | Function -> let info = Callable_info.from_baseinfo base_info
      in Some info
    | _ -> None

let callable_test fn =
  match get_callable_info () with
  | None -> assert_equal_string func_name "No base info found"
  | Some (info) -> fn info

let test_can_throw_gerror _ =
  callable_test (fun info ->
      let throw_error = Callable_info.can_throw_gerror info in
      assert_equal_boolean false throw_error
    )

let test_get_n_args _ =
  callable_test (fun info ->
      let n = Callable_info.get_n_args info in
      assert_equal_int 1 n
    )

let test_get_return_attribute _ =
  callable_test (fun info ->
      match Callable_info.get_return_attribute info with
      | None -> assert_equal_boolean true true
      | Some attribute -> assert_equal_string "This should not return a string" attribute
    )

let test_is_method _ =
  callable_test (fun info ->
      let is_method = Callable_info.is_method info in
      assert_equal_boolean false is_method
    )

let test_may_return_null _ =
  callable_test (fun info ->
      let may_return_null = Callable_info.may_return_null info in
      assert_equal_boolean false may_return_null
    )

let test_skip_return _ =
  callable_test (fun info ->
      let skip_return = Callable_info.skip_return info in
      assert_equal_boolean false skip_return
    )

let test_get_caller_owns _ =
  callable_test (fun info ->
      let transfer = Callable_info.get_caller_owns info in
      assert_equal ~printer:(fun t ->
          match t with
          | Bindings.Arg_info.Nothing -> "nothing"
          | Bindings.Arg_info.Container -> "container"
          | Bindings.Arg_info.Everything -> "everything"
        ) Bindings.Arg_info.Nothing transfer
    )

let tests =
  "GObject Introspection CallableInfo tests" >:::
  [
    "Callable_info can throw gerror" >:: test_can_throw_gerror;
    "Callable_info get n args" >:: test_get_n_args;
    "Callable_info get return attribute" >:: test_get_return_attribute;
    "Callable_info is method" >:: test_is_method;
    "Callable_info may return null" >:: test_may_return_null;
    "Callable_info skip return" >:: test_skip_return;
    "Callable_info get caller owns" >:: test_get_caller_owns
  ]
