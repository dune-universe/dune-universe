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
let object_name = "Window"
let signal_name = "activate-default"

let get_signal_info signal_name =
  match Repository.find_by_name namespace object_name with
  | None -> None
  | Some (base_info) ->
    match Base_info.get_type base_info with
    | Object -> (
        let object_info = Object_info.from_baseinfo base_info in
        match Object_info.find_signal object_info signal_name with
        | None -> None
        | Some info' -> Some info'
      )
    | _ -> None

let signal_activate_default_test fn =
  match get_signal_info signal_name with
  | None -> assert_equal_string object_name "No base info found"
  | Some (info) -> fn info

let test_true_stops_emit _ =
  signal_activate_default_test (fun info ->
      let stops = Signal_info.true_stops_emit info in
      assert_equal_boolean false stops
    )

let test_get_class_closure _ =
  signal_activate_default_test (fun info ->
      match Signal_info.get_class_closure info with
      | None -> assert_equal_boolean true true
      | Some _ -> assert_equal_string "Class closure " " found"
    )

let test_get_multiple_flags _ =
  signal_activate_default_test (fun info ->
      let flags = Signal_info.get_flags info in
      let len = List.length flags in
      let _ = assert_equal_int 2 len in
      let _ = assert_bool "Run last" (List.mem Bindings.GSignal.Run_last flags) in
      assert_bool "Action" (List.mem Bindings.GSignal.Action flags)
    )

let signal_name = "keys-changed"

let signal_keys_changed_test fn =
  match get_signal_info signal_name with
  | None -> assert_equal_string object_name "No base info found"
  | Some (info) -> fn info

let test_get_simple_flags _ =
  signal_keys_changed_test (fun info ->
      let flags = Signal_info.get_flags info in
      let len = List.length flags in
      let _ = assert_equal_int 1 len in
      assert_bool "Run First" (List.mem Bindings.GSignal.Run_first flags)
    )

let tests =
  "GObject Introspection SignalInfo tests" >:::
  [
    "Signal_info true stops emit" >:: test_true_stops_emit;
    "GISingalInfo get class closure" >:: test_get_class_closure;
    "Signal_info get multiple flags" >:: test_get_multiple_flags;
    "Signal_info get simple flags" >:: test_get_simple_flags;
  ]
