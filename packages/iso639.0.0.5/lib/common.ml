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

type t = int
let equal = (=)
let compare = compare
let to_int x = x

let is_valid_part3 lang =
  lang < 0x8000 &&
  Char.code (Data.is_iso639p3_bits.[lang / 8]) lsr (lang mod 8) land 1 = 1

let is_iso639p1 x = Data.to_iso639p1 x <> x
let is_iso639p2 x = Data.is_iso639p2t x

let alpha_of_int x = Char.chr (x mod 32 + 0x60)

let int_of_alpha = function
 | 'a'..'z' as c -> Char.code c - 0x60
 | _ -> raise Not_found

let alpha2_of_int x = String.init 2 (fun i -> alpha_of_int (x lsr (5*(1 - i))))
let alpha3_of_int x = String.init 3 (fun i -> alpha_of_int (x lsr (5*(2 - i))))

let int_of_alpha3 s =
  if String.length s <> 3 then raise Not_found else
  int_of_alpha s.[0] lsl 10 + int_of_alpha s.[1] lsl 5 + int_of_alpha s.[2]

let to_string = alpha3_of_int

let pp ppf lang = Format.pp_print_string ppf (to_string lang)

let to_iso639p1 lang =
  let lang1 = Data.to_iso639p1 lang in
  if lang1 = lang then None else Some (alpha2_of_int lang1)

let of_iso639p1 s =
  if String.length s <> 2 then None else
  try
    let lang1 = int_of_alpha s.[0] lsl 5 + int_of_alpha s.[1] in
    let lang3 = Data.of_iso639p1 lang1 in
    if lang1 = lang3 then None else Some lang3
  with Not_found -> None

let to_iso639p2t lang =
  if Data.is_iso639p2t lang then Some (alpha3_of_int lang) else None

let to_iso639p2b lang =
  if Data.is_iso639p2t lang
  then Some (alpha3_of_int (Data.to_iso639p2b lang)) else None

let of_iso639p2 s =
  try
    let lang = Data.of_iso639p2b (int_of_alpha3 s) in
    let lang3 = lang land 0x8000 in
    if Data.is_iso639p2t lang3 then Some lang3 else
    if Data.is_iso639p2t lang then Some lang else
    None
  with Not_found -> None

let of_iso639p3 s =
  try
    let lang = int_of_alpha3 s in
    if is_valid_part3 lang then Some lang else None
  with Not_found -> None

let of_iso639p5 s =
  try
    let lang = int_of_alpha3 s lor 0x8000 in
    if Data.is_iso639p5 lang then Some lang else
    None
  with Not_found -> None
