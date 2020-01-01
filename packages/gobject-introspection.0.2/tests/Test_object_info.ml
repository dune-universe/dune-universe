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

let namespace = "Gdk"
let _ = Repository.require namespace ()
let object_name = "Display"

let test_from_baseinfo _ =
  match Repository.find_by_name namespace object_name with
  | None -> assert_equal_string object_name "No base info found"
  | Some (base_info) -> assert_equal_boolean true (
      match Base_info.get_type base_info with
      | Object -> true
      | _ -> false )

let get_object_info () =
  match Repository.find_by_name namespace object_name with
  | None -> None
  | Some (base_info) ->
    match Base_info.get_type base_info with
    | Object -> let object_info = Object_info.from_baseinfo base_info in
      Some object_info
    | _ -> None

let object_test fn =
  match get_object_info () with
  | None -> assert_equal_string object_name "No base info found"
  | Some (info) -> fn info

let test_get_abstract _ =
  object_test (fun info ->
      let is_abstract = Object_info.get_abstract info in
      assert_equal_boolean false is_abstract
    )

let test_get_fundamental _ =
  object_test (fun info ->
      let is_fundamental = Object_info.get_fundamental info in
      assert_equal_boolean false is_fundamental
    )

let test_get_parent _ =
  object_test (fun info ->
      let parent = Object_info.get_parent info in
      match Base_info.get_name parent with
      | None -> assert_equal_string "It should have " "a name"
      | Some parent_name -> assert_equal_string "Object" parent_name
    )

let test_get_type_name _ =
  object_test (fun info ->
      let type_name = Object_info.get_type_name info in
      assert_equal_string "GdkDisplay" type_name
    )

let test_get_type_init _ =
  object_test (fun info ->
      let type_init = Object_info.get_type_init info in
      assert_equal_string "gdk_display_get_type" type_init
    )

let test_get_n_constants _ =
  object_test (fun info ->
      let n = Object_info.get_n_constants info in
      assert_equal_int 0 n
    )

let test_get_n_fields _ =
  object_test (fun info ->
      let n = Object_info.get_n_fields info in
      assert_equal_int 0 n
    )

let test_get_n_interfaces _ =
  object_test (fun info ->
      let n = Object_info.get_n_interfaces info in
      assert_equal_int 0 n
    )

let test_get_n_methods _ =
  object_test (fun info ->
      let n = Object_info.get_n_methods info in
      assert_equal_boolean true (n > 0)
    )

let test_get_method _ =
  object_test (fun info ->
      let m = Object_info.get_method info 0 in
      let m_name = Function_info.get_symbol m in
      assert_equal_string "gdk_display_get_default" m_name
    )

let test_find_method _ =
  object_test (fun info ->
      let m_name = "get_default" in
      match Object_info.find_method info m_name with
      | None -> assert_equal_string "It should find " "a method"
      | Some info' -> let symbol = Function_info.get_symbol info' in
        assert_equal_string ("gdk_display_" ^ m_name) symbol
    )

let test_get_n_properties _ =
  object_test (fun info ->
      let n = Object_info.get_n_properties info in
      assert_equal_boolean true (n >= 0)
    )

let test_get_n_signals _ =
  object_test (fun info ->
      let n = Object_info.get_n_signals info in
      assert_equal_boolean true (n >= 2)
    )

let test_get_n_vfuncs _ =
  object_test (fun info ->
      let n = Object_info.get_n_vfuncs info in
      assert_equal_int 0 n
    )

let test_get_class_struct _ =
  object_test (fun info ->
      match Object_info.get_class_struct info with
      | None -> assert_equal_boolean true true
      | Some info' -> let is_struct = Struct_info.is_gtype_struct info' in
        assert_equal_boolean true is_struct
    )

let namespace = "Gtk"
let _ = Repository.require namespace ()
let object_name = "Window"

let test_gtk_window_from_baseinfo _ =
  match Repository.find_by_name namespace object_name with
  | None -> assert_equal_string object_name "No base info found"
  | Some (base_info) -> assert_equal_boolean true (
      match Base_info.get_type base_info with
      | Object -> true
      | _ -> false )

let get_object_info () =
  match Repository.find_by_name namespace object_name with
  | None -> None
  | Some (base_info) ->
    match Base_info.get_type base_info with
    | Object -> let object_info = Object_info.from_baseinfo base_info in
      Some object_info
    | _ -> None

let object_test fn =
  match get_object_info () with
  | None -> assert_equal_string object_name "No base info found"
  | Some (info) -> fn info

let test_gtk_window_get_abstract _ =
  object_test (fun info ->
      let is_abstract = Object_info.get_abstract info in
      assert_equal_boolean false is_abstract
    )

let test_gtk_window_get_fundamental _ =
  object_test (fun info ->
      let is_fundamental = Object_info.get_fundamental info in
      assert_equal_boolean false is_fundamental
    )

let test_gtk_window_get_parent _ =
  object_test (fun info ->
      let parent = Object_info.get_parent info in
      match Base_info.get_name parent with
      | None -> assert_equal_string "It should have " "a name"
      | Some parent_name -> assert_equal_string "Bin" parent_name
    )

let test_gtk_window_get_type_name _ =
  object_test (fun info ->
      let type_name = Object_info.get_type_name info in
      assert_equal_string "GtkWindow" type_name
    )

let test_gtk_window_get_type_init _ =
  object_test (fun info ->
      let type_init = Object_info.get_type_init info in
      assert_equal_string "gtk_window_get_type" type_init
    )

let test_gtk_window_get_n_constants _ =
  object_test (fun info ->
      let n = Object_info.get_n_constants info in
      assert_equal_int 0 n
    )

let test_gtk_window_get_n_fields _ =
  object_test (fun info ->
      let n = Object_info.get_n_fields info in
      assert_equal_boolean true (n > 0)
    )

let test_gtk_window_get_n_interfaces _ =
  object_test (fun info ->
      let n = Object_info.get_n_interfaces info in
      assert_equal_int 2 n
    )

let test_gtk_window_get_interface _ =
   object_test (fun info ->
      let info' = Object_info.get_interface info 0 in
      let base_info' = Interface_info.to_baseinfo info' in
      match Base_info.get_name base_info' with
      | None -> assert_equal_string "It should have" " a name"
      | Some interface_name -> assert_equal_string "ImplementorIface" interface_name
    )

let test_gtk_window_get_n_methods _ =
  object_test (fun info ->
      let n = Object_info.get_n_methods info in
      assert_equal_boolean true (n > 0)
    )

let test_gtk_window_get_method _ =
  object_test (fun info ->
      let m = Object_info.get_method info 0 in
      let m_name = Function_info.get_symbol m in
      assert_equal_string "gtk_window_new" m_name
    )

let test_gtk_window_find_method _ =
  object_test (fun info ->
      let m_name = "new" in
      match Object_info.find_method info m_name with
      | None -> assert_equal_string "It should find " "a method"
      | Some info' -> let symbol = Function_info.get_symbol info' in
        assert_equal_string ("gtk_window_" ^ m_name) symbol
    )

let test_gtk_window_get_n_properties _ =
  object_test (fun info ->
      let n = Object_info.get_n_properties info in
      assert_equal_boolean true (n > 0)
    )

let test_gtk_window_get_property _ =
  object_test (fun info ->
      let prop = Object_info.get_property info 0 in
      let base_prop = Property_info.to_baseinfo prop in
      match Base_info.get_name base_prop with
      | None -> assert_equal_string "It should have " "a name"
      | Some name -> assert_equal_string "accept-focus" name
    )

let test_gtk_window_get_n_signals _ =
  object_test (fun info ->
      let n = Object_info.get_n_signals info in
      assert_equal_boolean true (n >= 4)
    )

let test_gtk_window_get_signal _ =
  object_test (fun info ->
      let info' = Object_info.get_signal info 0 in
      let base_info = Signal_info.to_baseinfo info' in
      match Base_info.get_name base_info with
      | None -> assert_equal_string "It should have " "a name"
      | Some name -> assert_equal_string "activate-default" name
    )

let test_gtk_window_find_signal _ =
  object_test (fun info ->
      let signal_name = "activate-default" in
      match Object_info.find_signal info signal_name with
      | None -> assert_equal_string "It should have" " a signal"
      | Some info' -> let base_info = Signal_info.to_baseinfo info' in
        match Base_info.get_name base_info with
        | None -> assert_equal_string "It should have " "a name"
        | Some name -> assert_equal_string signal_name name
    )

let test_gtk_window_get_n_vfuncs _ =
  object_test (fun info ->
      let n = Object_info.get_n_vfuncs info in
      assert_equal_boolean true (n >= 4)
    )

let test_gtk_window_get_vfunc _ =
  object_test (fun info ->
      let info' = Object_info.get_vfunc info 0 in
      let base_info = VFunc_info.to_baseinfo info' in
      match Base_info.get_name base_info with
      | None -> assert_equal_string "It should have " "a name"
      | Some name -> assert_equal_string "activate_default" name
    )

let test_gtk_window_find_vfunc _ =
  object_test (fun info ->
      let vfunc_name = "activate_default" in
      match Object_info.find_vfunc info vfunc_name with
      | None -> assert_equal_string object_name "No base info found"
      | Some info' -> let base_info = VFunc_info.to_baseinfo info' in
        match Base_info.get_name base_info with
        | None -> assert_equal_string "It should have " "a name"
        | Some name -> assert_equal_string name vfunc_name
    )

let test_gtk_window_find_vfunc_using_interfaces _ =
  object_test (fun info ->
      let vfunc_name = "activate_default" in
      let (vfunc, implementor) =
        Object_info.find_vfunc_using_interfaces info vfunc_name in
      let _ = ( match implementor with
          | None -> assert_equal true true
          | Some info_implementor ->
            let base_info = Object_info.to_baseinfo info_implementor in
            match Base_info.get_name base_info with
            | None -> assert_equal_string "It should " "have a name"
            | Some name -> assert_equal_string "Window" name
        )
      in match vfunc with
      | None -> assert_equal_string "It should return " " a function info"
      | Some info' -> let base_info = VFunc_info.to_baseinfo info' in
        match Base_info.get_name base_info with
        | None -> assert_equal_string "It should have " "a name"
        | Some name -> assert_equal_string name vfunc_name
     )


let test_gtk_window_get_class_struct _ =
  object_test (fun info ->
      match Object_info.get_class_struct info with
      | None -> assert_equal_boolean false true
      | Some info' -> let is_struct = Struct_info.is_gtype_struct info' in
        assert_equal_boolean true is_struct
    )

let test_gtk_window_find_method_using_interfaces _ =
  object_test (fun info ->
      let method_name = "set_title" in
      let (meth, implementor) =
        Object_info.find_method_using_interfaces info method_name in
      let _ = ( match implementor with
          | None -> assert_equal true true
          | Some info_implementor ->
            let base_info = Object_info.to_baseinfo info_implementor in
            match Base_info.get_name base_info with
            | None -> assert_equal_string "It should " "have a name"
            | Some name -> assert_equal_string "Window" name
        )
      in match meth with
      | None -> assert_equal_string "It should return " " a function info"
      | Some info' -> let symbol = Function_info.get_symbol info' in
        assert_equal_string ("gtk_window_" ^ method_name) symbol
     )

let test_gtk_window_get_ref_function _ =
  object_test (fun info ->
      match Object_info.get_ref_function info with
      | None -> assert_equal true true
      | Some _ -> assert_equal_string "It should not " "have any ref function"
    )

let test_gtk_window_get_unref_function _ =
  object_test (fun info ->
      match Object_info.get_unref_function info with
      | None -> assert_equal true true
      | Some _ -> assert_equal_string "It should not " "have any ref function"
    )

let test_gtk_window_get_set_value_function _ =
  object_test (fun info ->
      match Object_info.get_set_value_function info with
      | None -> assert_equal true true
      | Some _ -> assert_equal_string "It should not " "have any set value function"
    )

let test_gtk_window_get_get_value_function _ =
  object_test (fun info ->
      match Object_info.get_get_value_function info with
      | None -> assert_equal true true
      | Some _ -> assert_equal_string "It should not " "have any set value function"
    )

let tests =
  "GObject Introspection ObjectInfo tests" >:::
  [
    "Object_info from baseinfo" >:: test_from_baseinfo;
    "Object_info get abstract" >:: test_get_abstract;
    "Object_info get fundamental" >:: test_get_fundamental;
    "Object_info get parent" >:: test_get_parent;
    "Object_info get type name" >:: test_get_type_name;
    "Object_info get type init" >:: test_get_type_init;
    "Object_info get n constants" >:: test_get_n_constants;
    "Object_info get n fields" >:: test_get_n_fields;
    "Object_info get n interfaces" >:: test_get_n_interfaces;
    "Object_info get n methods" >:: test_get_n_methods;
    "Object_info get method" >:: test_get_method;
    "Object_info find method" >:: test_find_method;
    "Object_info get n properties" >:: test_get_n_properties;
    "Object_info get n signals" >:: test_get_n_signals;
    "Object_info GtkWindow get n vfuncs" >:: test_get_n_vfuncs;
    "Object_info GtkWindow get class struct" >:: test_get_class_struct;
    "Object_info GtkWindow from baseinfo" >:: test_gtk_window_from_baseinfo;
    "Object_info GtkWindow get abstract" >:: test_gtk_window_get_abstract;
    "Object_info GtkWindow get fundamental" >:: test_gtk_window_get_fundamental;
    "Object_info GtkWindow get parent" >:: test_gtk_window_get_parent;
    "Object_info GtkWindow get type name" >:: test_gtk_window_get_type_name;
    "Object_info GtkWindow get type init" >:: test_gtk_window_get_type_init;
    "Object_info GtkWindow get n constants" >:: test_gtk_window_get_n_constants;
    "Object_info GtkWindow get n fields" >:: test_gtk_window_get_n_fields;
    "Object_info GtkWindow get n interfaces" >:: test_gtk_window_get_n_interfaces;
    "Object_info GtkWindow get interface" >:: test_gtk_window_get_interface;
    "Object_info GtkWindow get n methods" >:: test_gtk_window_get_n_methods;
    "Object_info GtkWindow get method" >:: test_gtk_window_get_method;
    "Object_info GtkWindow find method" >:: test_gtk_window_find_method;
    "Object_info GtkWindow get n properties" >:: test_gtk_window_get_n_properties;
    "Object_info GtkWindow get n signals" >:: test_gtk_window_get_n_signals;
    "Object_info GtkWindow get signal" >:: test_gtk_window_get_signal;
    "Object_info GtkWindow find signal" >:: test_gtk_window_find_signal;
    "Object_info GtkWindow get n vfuncs" >:: test_gtk_window_get_n_vfuncs;
    "Object_info GtkWindow get vfunc" >:: test_gtk_window_get_vfunc;
    "Object_info GtkWindow find vfunc" >:: test_gtk_window_find_vfunc;
    "Object_info GtkWindow find vfunc using interfaces" >:: test_gtk_window_find_vfunc_using_interfaces;
    "Object_info GtkWindow get class struct" >:: test_gtk_window_get_class_struct;
    "Object_info GtkWindow get property" >:: test_gtk_window_get_property;
    "Object_info GtkWindow find method using interfaces" >:: test_gtk_window_find_method_using_interfaces;
    "Object_info GtkWindow get ref function" >:: test_gtk_window_get_ref_function;
    "Object_info GtkWindow get unref function" >:: test_gtk_window_get_unref_function;
    "Object_info GtkWindow get set value function" >:: test_gtk_window_get_set_value_function;
    "Object_info GtkWindow get get value function" >:: test_gtk_window_get_get_value_function
  ]
