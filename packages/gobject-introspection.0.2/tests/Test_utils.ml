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

let assert_equal_string str1 str2 =
  assert_equal ~printer: (fun s -> s) str1 str2

let assert_includes_string str1 str2 =
  let re = Str.regexp_string str2 in
  let result = try
      (Str.search_forward re str1 0) > -1
    with Not_found -> false
  in
  assert_bool (Printf.sprintf "%s should have included %s" str1 str2) result

let assert_equal_boolean bool1 bool2 =
  assert_equal ~printer: (fun s -> string_of_bool s) bool1 bool2

let assert_equal_int int1 int2 =
  assert_equal ~printer: (fun s -> string_of_int s) int1 int2

let assert_equal_or_greater int1 int2 =
  assert_equal ~printer: (fun _ ->
    String.concat " " [string_of_int int1;
                       "is not >=";
                       string_of_int int2]
  ) true (int1 >= int2)

let is_travis = try
  bool_of_string (Sys.getenv "TRAVIS_TESTS")
  with _ -> false

let assert_file_exists filename =
  assert_equal_boolean true (Sys.file_exists filename)

let file_content_to_string in_ch =
  let rec read_line acc =
    try
      let line = input_line in_ch in read_line (line :: acc)
    with
      End_of_file -> acc
  in let lines = List.rev (read_line []) in
  String.concat "\n" lines

let check_file_and_content name content =
  assert_file_exists name;
  let input_ch = open_in name in
  let lines = file_content_to_string input_ch in
  let _ = assert_equal_string content lines in
  close_in input_ch
