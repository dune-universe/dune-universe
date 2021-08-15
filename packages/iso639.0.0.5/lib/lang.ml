(* Copyright (C) 2018  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

include Common

let of_int lang =
  if is_valid_part3 lang then Some lang else None

let of_int_exn lang =
  if is_valid_part3 lang then lang else failwith "Iso639.Lang.of_int_exn"

let of_lang_or_family lang =
  let i = Lang_or_family.to_int lang in
  if i < 0x8000 then Some i else None

let to_lang_or_family = Lang_or_family.of_int_unsafe

let of_string = of_iso639p3

let of_string_exn s =
  (match of_iso639p3 s with
   | None -> invalid_arg "Iso639.Lang.of_string_exn"
   | Some lang -> lang)

let scope = Data.lang3_scope

let macrolanguage lang =
  let langM = Data.lang3_macrolanguage lang in
  if langM = lang then None else Some langM

let list_init n f = (* List.init was introduced in OCaml 4.06.0 *)
  let rec loop n acc =
    if n = 0 then acc else
    loop (n - 1) (f (n - 1) :: acc)
  in
  loop n []

let macrolanguage_members lang =
  let s = Data.lang3_macrolanguage_members lang in
  list_init (String.length s / 2)
    (fun i -> Char.code s.[2*i] lsl 8 lor Char.code s.[2*i + 1])
