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

open GObject_introspection

let () =
  let namespace = "Gtk" in
  print_endline "Search path";
  let sp = Repository.get_search_path () in
  List.iter (fun x -> print_endline ("\t -" ^ x)) sp;
  let _ = Repository.require namespace () in
  let c_prefix = Repository.get_c_prefix namespace in
(*  let shared_lib = Repository.get_shared_library repo namespace in *)
  let typelib_path = Repository.get_typelib_path namespace in
  let version = Repository.get_version namespace in
  print_endline ("Prefix:         " ^ c_prefix);
  (* print_endline ("Shared library: " ^ shared_lib); *)
  print_endline ("Typelib path  : " ^ typelib_path);
  print_endline ("Version:        " ^ version);
  print_endline ("Versions:       ");
  let versions = Repository.enumerate_versions namespace in
  List.iter (fun x -> print_endline ("\t v. : " ^ x)) versions;
  print_endline "Dependencies";
  let dependencies = Repository.get_dependencies namespace in
  List.iter (fun x -> print_endline ("\t dep. : " ^ x)) dependencies;
  print_endline "Immediate Dependencies";
(*  let dependencies = Repository.get_immediate_dependencies (Some repo) namespace in
  List.iter (fun x -> print_endline ("\t dep. : " ^ x)) dependencies; *)
  print_endline "Loaded namespaces";
  let namespaces = Repository.get_loaded_namespaces () in
  List.iter (fun x -> print_endline ("\t namespace : " ^ x)) namespaces;
  let n_infos = Repository.get_n_infos namespace in
  print_endline ("Number of elements : " ^ (string_of_int n_infos))
