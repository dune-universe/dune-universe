(*
   po_examples.ml - implementations of partial order examples

   Copyright (C) 2001-2002  Markus Mottl  (OEFAI)
   email: markus.mottl@gmail.com
   WWW:   http://www.ocaml.info

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*)

open Format

module MakePONTuple (Spec : sig val choices : int end) = struct
  type el = int * int
  type ord = Unknown | Lower | Equal | Greater

  let compare (x1, x2) (y1, y2) =
    if x1 < y1 then
      if x2 <= y2 then Lower
      else Unknown
    else if x1 > y1 then
      if x2 < y2 then Unknown
      else Greater
    else (* x1 = y1 *)
      if x2 < y2 then Lower
      else if x2 > y2 then Greater
      else Equal

  let rand_el () = Random.int Spec.choices, Random.int Spec.choices
  let pretty_print ppf (x, y) = fprintf ppf "(%d, %d)" x y
end

module MakePONList (Spec : sig val len : int val choices : int end) = struct
  type el = int list
  type ord = Unknown | Lower | Equal | Greater

  let rec compare l1 l2 acc = match l1, l2 with
    | h1 :: t1, h2 :: t2 ->
        if h1 < h2 then
          if acc = Greater then Unknown
          else compare t1 t2 Lower
        else if h1 > h2 then
          if acc = Lower then Unknown
          else compare t1 t2 Greater
        else compare t1 t2 acc
    | [], [] -> acc
    | _ -> failwith "PONList.compare: lists have different length"

  let compare l1 l2 = compare l1 l2 Equal

  let rand_el () =
    let l = ref [] in
    for _i = 1 to Spec.len do l := Random.int Spec.choices :: !l done;
    !l

  let pretty_print ppf l =
    let rec aux ppf = function
      | [] -> ()
      | [x] -> fprintf ppf "%d" x
      | h :: t -> fprintf ppf "%d, %a" h aux t in
    fprintf ppf "[%a]" aux l
end

module MakePOBList (Spec : sig val len : int end) = struct
  type el = bool list
  type ord = Unknown | Lower | Equal | Greater

  let rec compare_loop l1 l2 acc =
    match l1, l2 with
    | h1 :: t1, h2 :: t2 ->
        if h1 then
          if h2 then compare_loop t1 t2 acc
          else if acc = Lower then Unknown
          else compare_loop t1 t2 Greater
        else if h2 then
          if acc = Greater then Unknown
          else compare_loop t1 t2 Lower
        else compare_loop t1 t2 acc
    | [], [] -> acc
    | _ -> failwith "POBList.compare_loop: lists have different length"

  let compare l1 l2 = compare_loop l1 l2 Equal

  let rand_el () =
    let l = ref [] in
    for _i = 1 to Spec.len do l := Random.bool () :: !l done;
    !l

  let int_of_bool b = if b then 1 else 0

  let pretty_print ppf l =
    let rec loop ppf = function
      | [] -> ()
      | [x] -> pp_print_int ppf (int_of_bool x)
      | h :: t -> fprintf ppf "%d, %a" (int_of_bool h) loop t in
    fprintf ppf "[%a]" loop l
end
