(* A functional client interface to ldap

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
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Ldap_types
open Ldap_protocol
open Lber
open Unix
open Sys

type msgid = Int32.t

type ld_socket = Ssl of Ssl.socket
                 | Plain of file_descr

type conn = {
  mutable rb: readbyte;
  mutable socket: ld_socket; (* communications channel to the ldap server *)
  mutable current_msgid: Int32.t; (* the largest message id allocated so far *)
  pending_messages: (int32, ldap_message Queue.t) Hashtbl.t;
  protocol_version: int;
}

type modattr = modify_optype * string * string list
type result = search_result_entry list
type entry = search_result_entry
type authmethod = [ `SIMPLE | `SASL ]
type search_result = [ `Entry of entry
                     | `Referral of (string list)
                     | `Success of (Ldap_types.ldap_controls option) ]
type page_control =
  [ `Noctrl
  | `Initctrl of int
  | `Subctrl of (int * string) ]

let ext_res = {ext_matched_dn="";
               ext_referral=None}

let _ = Ssl.init ()

(* limits us to Int32.max_int active async operations
   at any one time *)
let find_free_msgid con =
  let msgid = con.current_msgid in
    (if msgid = Int32.max_int then
       con.current_msgid <- 0l
     else
       con.current_msgid <- Int32.succ con.current_msgid);
    msgid

(* allocate a message id from the free message id pool *)
let allocate_messageid con =
  let msgid = find_free_msgid con in
    Hashtbl.replace con.pending_messages msgid (Queue.create ());
    msgid

let free_messageid con msgid =
  try Hashtbl.remove con.pending_messages msgid
  with Not_found ->
    raise (LDAP_Failure (`LOCAL_ERROR, "free_messageid: invalid msgid", ext_res))

(* send an ldapmessage *)
let send_message con msg =
  let write ld_socket buf off len =
    match ld_socket with
        Ssl s ->
          (try Ssl.write s buf off len
           with Ssl.Write_error _ -> raise (Unix_error (EPIPE, "Ssl.write", "")))
      | Plain s -> Unix.write s buf off len
  in
  let e_msg = Ldap_protocol.encode_ldapmessage msg in
  let e_msg = Bytes.of_string e_msg in
  let len = Bytes.length e_msg in
  let written = ref 0 in
    try
      while !written < len
      do
        written := ((write con.socket e_msg
                       !written (len - !written)) +
                    !written)
      done
    with
        Unix_error (EBADF, _, _)
      | Unix_error (EPIPE, _, _)
      | Unix_error (ECONNRESET, _, _)
      | Unix_error (ECONNABORTED, _, _)
      | Sys_error _ ->
          (raise
             (LDAP_Failure
                (`SERVER_DOWN,
                 "the connection object is invalid, data cannot be written",
                 ext_res)))

(* recieve an ldapmessage for a particular message id (messages for
   all other ids will be read and queued. They can be retreived later) *)
let receive_message con msgid =
  let q_for_msgid con msgid =
    try Hashtbl.find con.pending_messages msgid
    with Not_found -> raise (LDAP_Failure (`LOCAL_ERROR, "invalid message id", ext_res))
  in
  let rec read_message con msgid =
    let msg = decode_ldapmessage con.rb in
      if msg.messageID = msgid then msg
      else
        (let q = q_for_msgid con msg.messageID in
           Queue.add msg q;
           read_message con msgid)
  in
  let q = q_for_msgid con msgid in
    try
      if Queue.is_empty q then
        read_message con msgid
      else Queue.take q
    with
        Readbyte_error Transport_error ->
          raise (LDAP_Failure (`SERVER_DOWN, "read error", ext_res))
      | Readbyte_error End_of_stream ->
          raise (LDAP_Failure (`LOCAL_ERROR, "bug in ldap decoder detected", ext_res))

exception Timeout

let init ?(connect_timeout = 1) ?(version = 3) hosts =
  if ((version < 2) || (version > 3)) then
    raise (LDAP_Failure (`LOCAL_ERROR, "invalid protocol version", ext_res))
  else
    let fd =
      let addrs =
        (List.flatten
           (List.map
              (fun (mech, host, port) ->
                 try
                   (List.rev_map
                      (fun addr -> (mech, addr, port))
                      (Array.to_list ((gethostbyname host).h_addr_list)))
                 with Not_found -> [])
              (List.map
                 (fun host ->
                    (match Ldap_url.of_string host with
                         {url_mech=mech;url_host=(Some host);url_port=(Some port);_} ->
                           (mech, host, int_of_string port)
                       | {url_mech=mech;url_host=(Some host);url_port=None;_} ->
                           (mech, host, 389)
                       | _ -> raise
                           (LDAP_Failure (`LOCAL_ERROR, "invalid ldap url", ext_res))))
                 hosts)))
      in
      let rec open_con addrs =
        let previous_signal = ref Signal_default in
          match addrs with
              (mech, addr, port) :: tl ->
                (try
                   if mech = `PLAIN then
                     let s = socket PF_INET SOCK_STREAM 0 in
                       try
                         previous_signal :=
                           signal sigalrm
                             (Signal_handle (fun _ -> raise Timeout));
                         ignore (alarm connect_timeout);
                         connect s (ADDR_INET (addr, port));
                         ignore (alarm 0);
                         set_signal sigalrm !previous_signal;
                         Plain s
                       with exn -> close s;raise exn
                   else
                     (previous_signal :=
                        signal sigalrm
                          (Signal_handle (fun _ -> raise Timeout));
                      ignore (alarm connect_timeout);
                      let ssl = Ssl (Ssl.open_connection
                                       Ssl.SSLv23
                                       (ADDR_INET (addr, port)))
                      in
                        ignore (alarm 0);
                        set_signal sigalrm !previous_signal;
                        ssl)
                 with
                     Unix_error (ECONNREFUSED, _, _)
                   | Unix_error (EHOSTDOWN, _, _)
                   | Unix_error (EHOSTUNREACH, _, _)
                   | Unix_error (ECONNRESET, _, _)
                   | Unix_error (ECONNABORTED, _, _)
                   | Ssl.Connection_error _
                   | Timeout ->
                       ignore (alarm 0);
                       set_signal sigalrm !previous_signal;
                       open_con tl)
            | [] -> raise (LDAP_Failure (`SERVER_DOWN, "", ext_res))
      in
        open_con addrs
    in
      {rb=(match fd with
               Ssl s -> Lber.readbyte_of_ssl s
             | Plain s -> Lber.readbyte_of_fd s);
       socket=fd;
       current_msgid=1l;
       pending_messages=(Hashtbl.create 3);
       protocol_version=version}

(* sync auth_method types between the two files *)
let bind_s ?(who = "") ?(cred = "") ?(auth_method = `SIMPLE) con =
  let _ = auth_method in (* TODO: usused?? *)
  let msgid = allocate_messageid con in
    (try
       send_message con
         {messageID=msgid;
          protocolOp=Bind_request
                       {bind_version=con.protocol_version;
                        bind_name=who;
                        bind_authentication=(Simple cred)};
          controls=None};
       match receive_message con msgid with
           {protocolOp=Bind_response {bind_result={result_code=`SUCCESS;_};_};_} -> ()
         | {protocolOp=Bind_response {bind_result=res;_};_} ->
             raise (LDAP_Failure
                      (res.result_code, res.error_message,
                       {ext_matched_dn=res.matched_dn;
                        ext_referral=res.ldap_referral}))
         | _ -> raise (LDAP_Failure (`LOCAL_ERROR, "invalid server response", ext_res))
     with exn -> free_messageid con msgid;raise exn);
    free_messageid con msgid

let search ?(base = "") ?(scope = `SUBTREE) ?(aliasderef=`NEVERDEREFALIASES)
  ?(sizelimit=0l) ?(timelimit=0l) ?(attrs = []) ?(attrsonly = false)
  ?(page_control = `Noctrl) con filter =
  let msgid = allocate_messageid con in
  let build_res_ctrl size cookie =
    {Ldap_types.criticality = false;
    Ldap_types.control_details=(`Paged_results_control {Ldap_types.size; Ldap_types.cookie})}
  in
  let controls = match (page_control) with
    | `Noctrl -> None
    | `Initctrl size | `Subctrl (size,_) when size < 1 ->
      raise (Ldap_types.LDAP_Failure(`LOCAL_ERROR, "invalid page size", ext_res))
    | `Initctrl size -> Some [(build_res_ctrl size "")]
    | `Subctrl (size,cookie) -> Some [(build_res_ctrl size cookie)]
   in
    try
      let e_filter = (try Ldap_filter.of_string filter
                      with _ ->
                        (raise
                           (LDAP_Failure
                              (`LOCAL_ERROR, "bad search filter", ext_res))))
      in
        send_message con
          {messageID=msgid;
           protocolOp=Search_request
                        {baseObject=base;
                         scope=scope;
                         derefAliases=aliasderef;
                         sizeLimit=sizelimit;
                         timeLimit=timelimit;
                         typesOnly=attrsonly;
                         filter=e_filter;
                         s_attributes=attrs};
           controls};
        msgid
    with exn -> free_messageid con msgid;raise exn

let get_search_entry con msgid =
  try
    match receive_message con msgid with
        {protocolOp=Search_result_entry e;_} -> `Entry e
      | {protocolOp=Search_result_reference r;_} -> `Referral r
      | {protocolOp=Search_result_done {result_code=`SUCCESS;_};_} ->
          raise (LDAP_Failure (`SUCCESS, "success", ext_res))
      | {protocolOp=Search_result_done res;_} ->
        raise (LDAP_Failure (res.result_code, res.error_message,
                             {ext_matched_dn=res.matched_dn;
                              ext_referral=res.ldap_referral}))
      | _ -> raise (LDAP_Failure (`LOCAL_ERROR, "unexpected search response", ext_res))
  with exn -> free_messageid con msgid;raise exn

let get_search_entry_with_controls con msgid =
  try
    match receive_message con msgid with
        {Ldap_types.protocolOp=Ldap_types.Search_result_entry e;_} -> `Entry e
      | {Ldap_types.protocolOp=Ldap_types.Search_result_reference r;_} -> `Referral r
      | {Ldap_types.protocolOp=Ldap_types.Search_result_done {Ldap_types.result_code=`SUCCESS;_};Ldap_types.controls=cntrls;_} ->
        `Success cntrls
      | {Ldap_types.protocolOp=Ldap_types.Search_result_done res;_} ->
        raise (Ldap_types.LDAP_Failure (res.Ldap_types.result_code, res.Ldap_types.error_message,
                             {Ldap_types.ext_matched_dn=res.Ldap_types.matched_dn;
                              ext_referral=res.Ldap_types.ldap_referral}))
      | _ -> raise (Ldap_types.LDAP_Failure (`LOCAL_ERROR, "unexpected search response", ext_res))
  with exn -> free_messageid con msgid;raise exn

let abandon con msgid =
  let my_msgid = allocate_messageid con in
    try
      free_messageid con msgid;
      send_message con
        {messageID=my_msgid;
         protocolOp=(Abandon_request msgid);
         controls=None}
    with exn -> free_messageid con my_msgid;raise exn

let search_s ?(base = "") ?(scope = `SUBTREE) ?(aliasderef=`NEVERDEREFALIASES)
  ?(sizelimit=0l) ?(timelimit=0l) ?(attrs = []) ?(attrsonly = false) con filter =
  let msgid = search ~base:base ~scope:scope ~aliasderef:aliasderef ~sizelimit:sizelimit
                ~timelimit:timelimit ~attrs:attrs ~attrsonly:attrsonly con filter
  in
  let result = ref [] in
    (try
       while true
       do
         result := (get_search_entry con msgid) :: !result
       done
     with
         LDAP_Failure (`SUCCESS, _, _) -> ()
       | LDAP_Failure (code, msg, ext) -> raise (LDAP_Failure (code, msg, ext))
       | exn -> (try abandon con msgid with _ -> ());raise exn);
    free_messageid con msgid;
    !result

let add_s con (entry: entry) =
  let msgid = allocate_messageid con in
    (try
       send_message con
         {messageID=msgid;
          protocolOp=Add_request entry;
          controls=None};
       match receive_message con msgid with
           {protocolOp=Add_response {result_code=`SUCCESS;_};_} -> ()
         | {protocolOp=Add_response res;_} ->
             raise (LDAP_Failure (res.result_code, res.error_message,
                                  {ext_matched_dn=res.matched_dn;
                                   ext_referral=res.ldap_referral}))
         | _ -> raise (LDAP_Failure (`LOCAL_ERROR, "invalid add response", ext_res))
     with exn -> free_messageid con msgid;raise exn);
    free_messageid con msgid

let delete_s con ~dn =
  let msgid = allocate_messageid con in
    (try
       send_message con
         {messageID=msgid;
          protocolOp=Delete_request dn;
          controls=None};
       match receive_message con msgid with
           {protocolOp=Delete_response {result_code=`SUCCESS;_};_} -> ()
         | {protocolOp=Delete_response res;_} ->
             raise (LDAP_Failure (res.result_code, res.error_message,
                                  {ext_matched_dn=res.matched_dn;
                                   ext_referral=res.ldap_referral}))
         | _ -> raise (LDAP_Failure (`LOCAL_ERROR, "invalid delete response", ext_res))
     with exn -> free_messageid con msgid;raise exn);
    free_messageid con msgid

let unbind con =
  try
    (match con.socket with
         Ssl s -> Ssl.shutdown s
       | Plain s -> close s)
  with _ -> ()

let modify_s con ~dn ~mods =
  let rec convertmods ?(converted=[]) mods =
    match mods with
        (op, attr, values) :: tl ->
          (convertmods
             ~converted:({mod_op=op;
                          mod_value={attr_type=attr;
                                     attr_vals=values}} :: converted)
             tl)
      | [] -> converted
  in
  let msgid = allocate_messageid con in
    (try
       send_message con
         {messageID=msgid;
          protocolOp=Modify_request
                       {mod_dn=dn;
                        modification=convertmods mods};
          controls=None};
       match receive_message con msgid with
           {protocolOp=Modify_response {result_code=`SUCCESS;_};_} -> ()
         | {protocolOp=Modify_response res;_} ->
             raise (LDAP_Failure (res.result_code, res.error_message,
                                  {ext_matched_dn=res.matched_dn;
                                   ext_referral=res.ldap_referral}))
         | _ -> raise (LDAP_Failure (`LOCAL_ERROR, "invalid modify response", ext_res))
     with exn -> free_messageid con msgid;raise exn);
    free_messageid con msgid

let modrdn_s ?(deleteoldrdn=true) ?(newsup=None) con ~dn ~newdn =
  let _ = newsup in (* TODO: not used?? *)
  let msgid = allocate_messageid con in
    (try
       send_message con
         {messageID=msgid;
          protocolOp=Modify_dn_request
                       {modn_dn=dn;
                        modn_newrdn=newdn;
                        modn_deleteoldrdn=deleteoldrdn;
                        modn_newSuperior=None};
          controls=None};
       match receive_message con msgid with
           {protocolOp=Modify_dn_response {result_code=`SUCCESS;_};_} -> ()
         | {protocolOp=Modify_dn_response res;_} ->
             raise (LDAP_Failure (res.result_code, res.error_message,
                                  {ext_matched_dn=res.matched_dn;
                                   ext_referral=res.ldap_referral}))
         | _ -> raise (LDAP_Failure (`LOCAL_ERROR, "invalid modify dn response", ext_res))
     with exn -> free_messageid con msgid;raise exn);
    free_messageid con msgid
