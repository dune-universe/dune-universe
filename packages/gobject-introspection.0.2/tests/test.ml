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

let () =
  run_test_tt_main
  ("GObjectIntrospection" >:::
    [
      Test_repository_default.tests;
      Test_repository.tests;
      Test_base_info.tests;
      Test_function_info.tests;
      Test_struct_info.tests;
      Test_union_info.tests;
      Test_field_info.tests;
      Test_enum_info.tests;
      Test_callable_info.tests;
      Test_arg_info.tests;
      Test_type_info.tests;
      Test_constant_info.tests;
      Test_object_info.tests;
      Test_interface_info.tests;
      Test_property_info.tests;
      Test_signal_info.tests;
      Test_vfunc_info.tests;
      Test_registered_type_info.tests;
      (* Test_version.tests; *)
    ]
  )
