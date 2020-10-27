(* An object oriented interface for parsing Lightweight Directory
   Interchange Format file

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


open Netencoding
open Ldap_ooclient
open Ldif_parser

let safe_string_regex =
  Str.regexp "^[\x01-\x09\x0b-\x0c\x0e-\x7f]+$"

let password_regex =
  Str.regexp_case_fold ".*p\\(ass\\)?w\\(or\\)?d$"

let empty_regex =
  Str.regexp "^ *$\\|^  *.*$"

let safe_val buf s =
  if
    (Str.string_match safe_string_regex s 0) &&
    (not (Str.string_match empty_regex s 0))
  then begin
    Buffer.add_string buf ": ";
    Buffer.add_string buf s
  end
  else begin
    Buffer.add_string buf ":: ";
    Buffer.add_string buf (Base64.encode s)
  end

let safe_attr_val buf a v =
  if Str.string_match password_regex a 0 then begin
    Buffer.add_string buf a;
    Buffer.add_string buf ":: ";
    Buffer.add_string buf (Base64.encode v)
  end
  else begin
    Buffer.add_string buf a;
    safe_val buf v
  end

let entry2ldif ?(ext=false) outbuf e =
  Buffer.add_string outbuf "dn";
  safe_val outbuf e#dn;
  if ext then Buffer.add_string outbuf "\nchangetype: add";
  Buffer.add_char outbuf '\n';
  (List.iter
     (fun attr ->
        (List.iter
           (fun value ->
              safe_attr_val outbuf attr value;
              Buffer.add_char outbuf '\n')
           (e#get_value attr)))
     e#attributes);
  Buffer.add_char outbuf '\n';
  outbuf

let iter (f: ('a -> unit)) ldif =
  try
    while true
    do
      f ldif#read_entry
    done
  with End -> ()

let fold f ldif v =
  let objects =
    let objects = ref [] in
      try
        while true
        do
          objects := (ldif#read_entry) :: !objects
        done;
        !objects
      with End -> !objects
  in
    List.fold_left f v objects

class ldif ?(in_ch=stdin) ?(out_ch=stdout) () =
object (_self)
  val in_ch  = {stream=(Stream.of_channel in_ch);buf=Buffer.create 256;line=1}
  val out_ch = out_ch
  val outbuf = Buffer.create 50

  method read_entry = Ldap_ooclient.to_entry (`Entry (ldif_attrval_record in_ch))

  method of_string s =
    let strm = {stream=(Stream.of_string s);buf=Buffer.create 256;line=1} in
      Ldap_ooclient.to_entry (`Entry (ldif_attrval_record strm))

  method to_string (e:ldapentry_t) =
    try
      let contents = Buffer.contents (entry2ldif outbuf e) in
        Buffer.clear outbuf;
        contents
    with exn ->
      Buffer.clear outbuf;
      raise exn

  method write_entry (e:ldapentry_t) =
    try
      Buffer.output_buffer out_ch (entry2ldif outbuf e);
      Buffer.clear outbuf
    with exn ->
      Buffer.clear outbuf;
      raise exn
end

let read_ldif_file file =
  let fd = open_in file in
    try
      let ldif = new ldif ~in_ch:fd () in
      let entries = fold (fun l e -> e :: l) ldif [] in
        close_in fd;
        entries
    with exn -> close_in fd;raise exn

let write_ldif_file file entries =
  let fd = open_out file in
    try
      let ldif = new ldif ~out_ch:fd () in
        List.iter ldif#write_entry entries;
        close_out fd
    with exn -> close_out fd;raise exn
