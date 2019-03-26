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
  if is_valid_part3 lang || Data.is_iso639p5 lang then Some lang else None

let of_int_exn lang =
  if is_valid_part3 lang || Data.is_iso639p5 lang then lang else
  invalid_arg "Iso639.of_int_exn"

let of_int_unsafe lang = lang

let is_iso639p3 x = x < 0x8000
let is_iso639p5 x = x >= 0x8000

let of_string s =
  try
    let lang = int_of_alpha3 s in
    if is_valid_part3 lang || Data.is_iso639p5 lang then Some lang else None
  with Not_found -> None

let to_iso639p3 lang =
  if is_iso639p3 lang then Some (alpha3_of_int lang) else None

let to_iso639p5 lang =
  if is_iso639p5 lang then Some (alpha3_of_int lang) else None

let scope x = if is_iso639p3 x then Data.lang3_scope x else `Collective
