(**************************************************************************)
(*                                                                        *)
(*   Typerex Libraries                                                    *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open EzCompat
open String

(* Character predicates *)
let for_all f s =
  let res = ref true in
  for i = 0 to length s - 1 do
    res := !res && f s.[i]
  done;
  !res

let exists f s =
  let res = ref false in
  for i = 0 to length s - 1 do
    res := !res || f s.[i]
  done;
  !res

(* The substring before [pos] (the character at position [pos] is discarded) *)
let before s pos =
  sub s 0 pos

(* The substring after [pos] (the character at position [pos] is discarded) *)
let after s pos =
  sub s (pos + 1) (length s - pos - 1)

let rec strneql s1 s2 len =
  len = 0 || (
    let len = len - 1 in
    s1.[len] = s2.[len] && strneql s1 s2 len)

let starts_with s ~prefix =
  let len1 = length s in
  let len2 = length prefix in
  len2 <= len1 && strneql s prefix len2

let ends_with s ~suffix =
  let rec aux offset len =
    len = 0 || (
      let len = len - 1 in
      s.[offset+len] = suffix.[len] && aux offset len) in
  let len1 = length s in
  let len2 = length suffix in
  len2 <= len1 && aux (len1 - len2) len2

(* Cut the string at position [pos] (the character at position [pos]
   is kept if [keep]) *)
let cut s pos =
  try
    before s pos, after s pos
  with _ -> s, ""

let cut_at s c =
  try
    let pos = index s c in
    cut s pos
  with _ -> s, ""

let rcut_at s c =
  try
    let pos = rindex s c in
    cut s pos
  with _ -> s, ""

let split s c =
  let len = String.length s in
  match String.index s c with
  | exception Not_found ->
    if len = 0 then [] else [ s ]
  | pos ->
    let rec iter pos to_rev =
      match String.index_from s pos c with
      | exception Not_found ->
        List.rev ( String.sub s pos ( len - pos ) :: to_rev )
      | pos2 ->
        iter ( pos2 + 1 )
          ( String.sub s pos ( pos2 - pos ) :: to_rev )
    in
    iter (pos+1) [ String.sub s 0 pos ]

let _ =
  assert (split "" 'o' = []);
  assert (split "o" 'o' = [""; ""]);
  assert (split "toto" 'o' = ["t"; "t"; ""]);
  assert (split "ototo" 'o' = [""; "t"; "t"; ""]);
  assert (split "tot" 'o' = ["t"; "t"]);
  ()

let split_simplify s c =
  let len = String.length s in
  let rec iter pos to_rev =
    if pos = len then List.rev to_rev else
      match try
              Some ( String.index_from s pos c )
        with Not_found -> None
      with
          Some pos2 ->
            if pos2 = pos then iter (pos+1) to_rev else
              iter (pos2+1) ((String.sub s pos (pos2-pos)) :: to_rev)
        | None -> List.rev ( String.sub s pos (len-pos) :: to_rev )
  in
  iter 0 []

let _ =
  assert (split_simplify "" 'o' = []);
  assert (split_simplify "toto" 'o' = ["t"; "t"]);
  assert (split_simplify "ototo" 'o' = ["t"; "t"]);
  assert (split_simplify "tot" 'o' = ["t"; "t"]);
  ()
