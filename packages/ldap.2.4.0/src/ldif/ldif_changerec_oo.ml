(* create an ldap changerec factory from a channel attached to an ldif
   changerec source default is stdin and stdout.

   Copyright (C) 2004 Eric Stokes, Matthew Backes, and The California
   State University at Northridge

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

open Ldap_ooclient
open Ldif_changerec_parser
open Ldif_changerec_lexer

exception Invalid_changerec of string
exception End_of_changerecs

let iter f cr =
  try
    while true
    do
      f cr#read_changerec
    done
  with End_of_changerecs -> ()

let rec fold f cr a =
  try fold f cr (f a cr#read_changerec)
  with End_of_changerecs -> a

let insert_change buf cr =
  match cr with
      `Modification (dn, mod_op) ->
        Buffer.add_string buf ("dn: " ^ dn ^ "\n");
        Buffer.add_string buf "changetype: modify\n";
        List.iter
          (fun (op, attr, vals) ->
             (match op with
                  `ADD -> Buffer.add_string buf ("add: " ^ attr ^ "\n")
                | `DELETE -> Buffer.add_string buf ("delete: " ^ attr ^ "\n")
                | `REPLACE -> Buffer.add_string buf ("replace: " ^ attr ^ "\n"));
             List.iter
               (fun valu -> Buffer.add_string buf (attr ^ ": " ^ valu ^ "\n"))
               vals;
             Buffer.add_string buf "-\n")
          mod_op;
        Buffer.add_string buf "\n";
        buf
    | `Addition e -> Ldif_oo.entry2ldif ~ext:true buf e;
    | `Delete dn ->
        Buffer.add_string buf ("dn: " ^ dn ^ "\n");
        Buffer.add_string buf "changetype: delete\n";
        buf
    | `Modrdn (dn, deleteoldrdn, newrdn) ->
        Buffer.add_string buf ("dn: " ^ dn ^ "\n");
        Buffer.add_string buf "changetype: modrdn\n";
        Buffer.add_string buf ("deleteoldrdn: " ^ (string_of_int deleteoldrdn) ^ "\n");
        Buffer.add_string buf ("newrdn: " ^ newrdn ^ "\n");
        buf

class change ?(in_ch=stdin) ?(out_ch=stdout) () =
object (self)
  val lxbuf = Lexing.from_channel in_ch
  val buf = Buffer.create 1
  method read_changerec =
    try changerec lexcr lxbuf
    with
        Failure "end" -> raise End_of_changerecs
      | Failure s -> raise (Invalid_changerec s)
  method of_string (s:string) =
    let lx = Lexing.from_string s in
      try changerec lexcr lx
      with
          Failure "end" -> raise End_of_changerecs
        | Failure s -> raise (Invalid_changerec s)
  method to_string (e:changerec) =
    let res = Buffer.contents (insert_change buf e) in
      Buffer.clear buf;res
  method write_changerec (e:changerec) =
    ignore (insert_change buf e);
    Buffer.output_buffer out_ch buf;
    Buffer.clear buf
end
