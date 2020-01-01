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

let get_info_strings info n=
  ["Base_info -> number:";
  string_of_int n;
  "name:";
  (match Base_info.get_name info with None -> "noname" | Some name -> name);
  "type:";
  (let t = Base_info.get_type info in
  Bindings.Base_info.string_of_info_type t) ]

let print_info namespace n =
  let message = let info = Repository.get_info namespace n in
    String.concat " " (get_info_strings info n)
  in print_endline message

let () =
  let namespace = "Gtk" in
  let _ = Repository.require namespace in
  let n = Repository.get_n_infos namespace in
  for i = 0 to (n - 1) do
    print_info namespace i;
  done
