(* Copyright 2021 Diskuv, Inc.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. *)
(** This module is meant to be opened during unit tests *)

module M = Dirsp_proscript_mirage.Make ()

let hexbuffer_equals = Alcotest.testable M.hexdump_pp M.equal

let hexbuffer_notequals =
  Alcotest.testable M.hexdump_pp (fun a b -> not (M.equal a b))

let strip_internal_whitespace = Str.global_replace (Str.regexp "[ \t\r\n]+") ""

(** [buffer_of_hex s] will ignore whitespace and convert a hexadecimal representation of bytes into a buffer. *)
let buffer_of_hex s =
  let s = strip_internal_whitespace s in
  let octets = (1 + String.length s) / 2 in
  Iter.(
    0 -- (octets - 1)
    |> map (fun octet ->
           let c1 = String.sub s (2 * octet) 1 in
           let c2 = String.sub s ((2 * octet) + 1) 1 in
           Scanf.sscanf (c1 ^ c2) "%02x" (fun x -> x)
           |> char_of_int |> M.elem_of_char)
    |> to_list)
  |> M.of_elem_list

let buffer_of_stripped_string s = M.of_string (strip_internal_whitespace s)

(** [polyfill_List_concat_map f l] gives the same result as {!concat}[ (]{!map}[ f l)], just the same as [List.concat_map f l] introduced in 4.10.0. Tail-recursive. *)
let polyfill_List_concat_map f l =
  let rec aux f acc = function
    | [] -> List.rev acc
    | x :: l ->
        let xs = f x in
        aux f (List.rev_append xs acc) l
  in
  aux f [] l
