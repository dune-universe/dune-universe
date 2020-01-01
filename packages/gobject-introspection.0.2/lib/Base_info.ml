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

open Ctypes
open Foreign
open Stubs

type t
let baseinfo : t structure typ = structure "Base_info"

let base_info_ref =
  foreign "g_base_info_ref" (ptr baseinfo @-> returning (ptr baseinfo))

let base_info_unref =
  foreign "g_base_info_unref" (ptr baseinfo @-> returning void)

let get_name =
  foreign "g_base_info_get_name" (ptr baseinfo @-> returning string_opt)

let equal =
  foreign "g_base_info_equal" (ptr baseinfo @-> ptr baseinfo @-> returning bool)

let get_namespace =
  foreign "g_base_info_get_namespace" (ptr baseinfo @-> returning string)

let is_deprecated =
  foreign "g_base_info_is_deprecated" (ptr baseinfo @-> returning bool)

let get_type =
  foreign "g_base_info_get_type"
    (ptr baseinfo @-> returning Stubs.Base_info.info_type)

let add_unref_finaliser info =
  let _ = Gc.finalise (fun i ->
      base_info_unref i
    ) info in
  info

let get_container info =
  let get_container_raw =
  foreign "g_base_info_get_container"
  (ptr baseinfo @-> returning (ptr_opt baseinfo)) in
  match get_container_raw info with
  | None -> None
  | Some container_info -> Some (add_unref_finaliser container_info)

(** GIRealInfo private struct only used to check ref_count and memory leaks *)
(* type realinfo
let realinfo : realinfo structure typ = structure "GIRealInfo"
let girealinfo_type = field realinfo "type" (int32_t)
let girealinfo_ref_count = field realinfo "ref_count" (int)
let () = seal realinfo

let realinfo_of_baseinfo info =
  coerce (ptr baseinfo) (ptr realinfo) info

let get_ref_count info =
  let realinfo = realinfo_of_baseinfo info in
  getf (!@realinfo) girealinfo_ref_count

let ref_count_file = (Sys.getcwd ()) ^ "/ref_count.log"
   *)
(* http://stackoverflow.com/questions/8090490/how-to-implement-appendfile-function *)
(*
let write_ref_count_log message =
  let oc = open_out_gen [Open_wronly; Open_append; Open_creat; Open_text] 0o640 ref_count_file in
  let _ = output_string oc message in
  close_out oc

let base_info_ref info =
  let base_info_ref_raw =
    foreign "g_base_info_ref" (ptr baseinfo @-> returning (ptr baseinfo))
  in
  let ref_count = get_ref_count info in
  let addr = raw_address_of_ptr (coerce (ptr baseinfo) (ptr void) info) in
  let name = match get_name info with
    | None -> "No name"
    | Some str -> str
  in
  let message = String.concat " " ["++ Ref count";
                                   Nativeint.to_string addr;
                                   string_of_int ref_count;
                                   "ref to  ";
                                   string_of_int (ref_count + 1);
                                   name;
                                   "\n"] in
  let _ = write_ref_count_log message in
  base_info_ref_raw info
*)

(*
let base_info_unref info =
  let base_info_unref_raw =
    foreign "g_base_info_unref" (ptr baseinfo @-> returning void)
  in
  let ref_count = get_ref_count info in
  let addr = raw_address_of_ptr (coerce (ptr baseinfo) (ptr void) info) in
  let name = match get_name info with
    | None -> "No name"
    | Some str -> str
  in
  let message = String.concat " " ["-- Ref count";
                                   Nativeint.to_string addr;
                                   string_of_int ref_count;
                                   "unref to";
                                   string_of_int (ref_count - 1);
                                   name;
                                   "\n"] in
  let _ = write_ref_count_log message in
  base_info_unref_raw info
*)
