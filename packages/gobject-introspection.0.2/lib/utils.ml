(*
 * Copyright 2019 Cedric LE MOIGNE, cedlemo@gmx.com
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

open Ctypes

let flags_list_of_int64 all_flags v =
  let open Int64 in
  let rec build_flags_list allf acc =
    match allf with
    | [] -> acc
    | (i, f) :: q -> if ((logand v i) <> zero) then build_flags_list q (f :: acc)
       else build_flags_list q acc
  in build_flags_list all_flags []

let int64_of_flags_list all_flags f =
  let open Int64 in
  let bitwise_or = fun acc value ->
    let (i, _f) = List.find (fun (i', f') -> value = f') all_flags in logor acc i
  in
  List.fold_left bitwise_or Int64.zero f

let generate_flags_list_view flags_ctyp all_flags =
  view flags_ctyp
    ~read:(flags_list_of_int64 all_flags)
    ~write:(int64_of_flags_list all_flags)
