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
let interface_name = "TlsServerConnection"

let get_interface_info interface_name =
  match Repository.find_by_name namespace interface_name with
  | None -> None
  | Some (base_info) ->
    match Base_info.get_type base_info with
    | Interface ->
      let interface_info = Interface_info.from_baseinfo base_info in
      Some interface_info
    | _ -> None

let interface_test fn =
  match get_interface_info interface_name with
  | None -> assert_equal_string interface_name "No base info found"
  | Some (info) -> fn info

let test_get_n_prerequisites _ =
  interface_test (fun info ->
      let n = Interface_info.get_n_prerequisites info in
      assert_equal_int 1 n
    )

let test_get_prerequisite _ =
  interface_test (fun info ->
      let info' = Interface_info.get_prerequisite info 0 in
      match Base_info.get_name info' with
      | None -> assert_equal_string "It should have " " a name"
      | Some name -> assert_equal_string "TlsConnection" name
    )

let test_get_n_properties _ =
  interface_test (fun info ->
      let n = Interface_info.get_n_properties info in
      assert_equal_int 1 n
    )

let test_get_property _ =
interface_test (fun info ->
      let info' = Interface_info.get_property info 0 in
      let base_info = Property_info.to_baseinfo info' in
      match Base_info.get_name base_info with
      | None -> assert_equal_string "It should have " " a name"
      | Some name -> assert_equal_string "authentication-mode" name
    )

let test_get_n_methods _ =
  interface_test (fun info ->
      let n = Interface_info.get_n_methods info in
      assert_equal_int 1 n
    )

let test_get_method _ =
  interface_test (fun info ->
      let info' = Interface_info.get_method info 0 in
      let base_info = Function_info.to_baseinfo info' in
      match Base_info.get_name base_info with
      | None -> assert_equal_string "It should have " " a name"
      | Some name -> assert_equal_string "new" name
    )

let test_find_method _ =
  interface_test (fun info ->
      let method_name = "new" in
      match Interface_info.find_method info method_name with
      | None -> assert_equal_string "It should " "return an info"
      | Some info' ->
      let symbol = Function_info.get_symbol info' in
      assert_equal_string ("g_tls_server_connection_"^ method_name) symbol
    )

let volume_interface = "Volume"

let volume_interface_test fn =
  match get_interface_info volume_interface with
  | None -> assert_equal_string interface_name "No base info found"
  | Some (info) -> fn info

let test_get_n_signals _ =
  volume_interface_test (fun info ->
      let n = Interface_info.get_n_signals info in
      assert_equal_int 2 n
    )

let test_get_signal _ =
  volume_interface_test (fun info ->
      let info' = Interface_info.get_signal info 0 in
      let base_info = Signal_info.to_baseinfo info' in
      match Base_info.get_name base_info with
      | None -> assert_equal_string "It should have " "a name"
      | Some name -> assert_equal_string "changed" name
    )

let test_find_signal _ =
  volume_interface_test (fun info ->
      let signal_name = "changed" in
      match Interface_info.find_signal info signal_name with
      | None -> assert_equal_string interface_name "No base info found"
      | Some info' -> let base_info = Signal_info.to_baseinfo info' in
        match Base_info.get_name base_info with
        | None -> assert_equal_string "It should have " "a name"
        | Some name -> assert_equal_string signal_name name
    )

let test_get_n_constants _ =
  volume_interface_test (fun info ->
      let n = Interface_info.get_n_constants info in
      assert_equal_int 0 n
    )

let test_get_iface_struct _ =
  volume_interface_test (fun info ->
      match Interface_info.get_iface_struct info with
      | None -> assert_equal_string "It would be " "great to have something"
      | Some struct_info -> let base_info = Struct_info.to_baseinfo struct_info in
        match Base_info.get_name base_info with
        | None -> assert_equal_string "It should have " "a name"
        | Some name -> assert_equal_string "VolumeIface" name
    )

let test_get_n_vfuncs _ =
  volume_interface_test (fun info ->
      let n = Interface_info.get_n_vfuncs info in
      assert_equal_int 21 n
    )

let test_get_vfunc _ =
  volume_interface_test (fun info ->
      let info' = Interface_info.get_vfunc info 0 in
      let base_info = VFunc_info.to_baseinfo info' in
      match Base_info.get_name base_info with
      | None -> assert_equal_string "It should have " "a name"
      | Some name -> assert_equal_string "can_eject" name
    )

let test_find_vfunc _ =
  volume_interface_test (fun info ->
      let vfunc_name = "can_eject" in
      match Interface_info.find_vfunc info vfunc_name with
      | None -> assert_equal_string interface_name "No base info found"
      | Some info' -> let base_info = VFunc_info.to_baseinfo info' in
        match Base_info.get_name base_info with
        | None -> assert_equal_string "It should have " "a name"
        | Some name -> assert_equal_string name vfunc_name
    )

let tests =
  "GObject Introspection InterfaceInfo tests" >:::
  [
    "Interface_info get n prerequisites" >:: test_get_n_prerequisites;
    "Interface_info get prerequisiste" >:: test_get_prerequisite;
    "Interface_info get n properties" >:: test_get_n_properties;
    "Interface_info get property" >:: test_get_property;
    "Interface_info get n methods" >:: test_get_n_methods;
    "Interface_info get method" >:: test_get_method;
    "Interface_info find method" >:: test_find_method;
    "Interface_info get n signals" >:: test_get_n_signals;
    "Interface_info get signal" >:: test_get_signal;
    "Interface_info find signal" >:: test_find_signal;
    "Interface_info get n constants" >:: test_get_n_constants;
    "Interface_info get iface struct" >:: test_get_iface_struct;
    "Interface_info get n vfuncs" >:: test_get_n_vfuncs;
    "Interface_info get vfunc" >:: test_get_vfunc;
    "Interface_info find vfunc" >:: test_find_vfunc
  ]
