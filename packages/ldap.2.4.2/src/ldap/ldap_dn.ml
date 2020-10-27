(* Utility functions for operating on dns

   Copyright (C) 2004 Eric Stokes, and The California State University
   at Northridge

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
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA
*)

open Ldap_types
open Ldap_dnlexer
open Printf

exception Invalid_dn of int * string

let of_string dn_string =
  let lexbuf = Lexing.from_string dn_string in
    try Ldap_dnparser.dn lexdn lexbuf
    with
        Parsing.Parse_error -> raise (Invalid_dn (lexbuf.Lexing.lex_curr_pos, "parse error"))
      | Failure msg -> raise (Invalid_dn (lexbuf.Lexing.lex_curr_pos, msg))

let hexpair_of_char c =
  let hexify i =
    match i with
        0 -> '0'
      | 1 -> '1'
      | 2 -> '2'
      | 3 -> '3'
      | 4 -> '4'
      | 5 -> '5'
      | 6 -> '6'
      | 7 -> '7'
      | 8 -> '8'
      | 9 -> '9'
      | 10 -> 'a'
      | 11 -> 'b'
      | 12 -> 'c'
      | 13 -> 'd'
      | 14 -> 'e'
      | 15 -> 'f'
      | n -> raise (Invalid_argument ("invalid hex digit: " ^ (string_of_int n)))
  in
  let i = int_of_char c in
  let buf = Bytes.create 2 in
    Bytes.set buf 0 (hexify (i lsr 4));
    Bytes.set buf 1 (hexify (i land 0b0000_1111));
    Bytes.to_string buf

let escape_value valu =
  let strm = Stream.of_string valu in
  let buf = Buffer.create ((String.length valu) + 10) in
  let rec escape strm buf =
    try
      match Stream.next strm with
          (',' | '=' | '+' | '<' | '>' | '#' | ';' | '\\' | '"') as c ->
            Buffer.add_char buf '\\';
            Buffer.add_char buf c;
          escape strm buf
        | ' ' ->
            if Stream.peek strm = None then begin
              Buffer.add_string buf "\\ ";
              escape strm buf
            end
            else begin
              Buffer.add_char buf ' ';
              escape strm buf
            end
        | c ->
            if (int_of_char c) < (int_of_char ' ') ||
               (int_of_char c) > (int_of_char '~')
            then begin
              Buffer.add_string buf ("\\" ^ (hexpair_of_char c));
              escape strm buf
            end
            else begin
              Buffer.add_char buf c;escape strm buf
            end
    with Stream.Failure -> Buffer.contents buf
  in
    match Stream.peek strm with
        Some ' ' ->
          Buffer.add_string buf "\\ ";
          Stream.junk strm;
          escape strm buf
      | Some _c -> escape strm buf
      | None -> ""

let to_string dn =
  let dn_to_strcomponents dn =
    List.map
      (fun {attr_type=attr;attr_vals=vals} ->
         let rec string_values s attr vals =
           match vals with
               valu :: [] -> sprintf "%s%s=%s" s attr (escape_value valu)
             | valu :: tl ->
                 string_values
                   (sprintf "%s%s=%s+"
                      s attr (escape_value valu))
                   attr tl
             | [] -> s
         in
           if List.length vals = 0 then
             raise
               (Invalid_dn
                  (0, "invalid dn structure. no attribute " ^
                     "value specified for attribute: " ^ attr))
           else
             string_values "" attr vals)
      dn
  in
  let rec components_to_dn s comps =
    match comps with
        comp :: [] -> sprintf "%s%s" s comp
      | comp :: tl -> components_to_dn (sprintf "%s%s," s comp) tl
      | [] -> s
  in
    components_to_dn "" (dn_to_strcomponents dn)

let canonical_dn dn = String.lowercase_ascii (to_string (of_string dn))
