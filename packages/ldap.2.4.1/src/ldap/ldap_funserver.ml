(*
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


open Lber
open Ldap_types
open Ldap_protocol
open Unix
open Printf

exception Server_error of string
exception Finished

type connection_id = int

type backendInfo = {
  bi_op_bind : (connection_id -> ldap_message -> ldap_message) option;
  bi_op_unbind : (connection_id -> ldap_message -> unit) option;
  bi_op_search : (connection_id -> ldap_message -> (unit -> ldap_message)) option;
  bi_op_compare : (connection_id -> ldap_message -> ldap_message) option;
  bi_op_modify : (connection_id -> ldap_message -> ldap_message) option;
  bi_op_modrdn : (connection_id -> ldap_message -> ldap_message) option;
  bi_op_add : (connection_id -> ldap_message -> ldap_message) option;
  bi_op_delete : (connection_id -> ldap_message -> ldap_message) option;
  bi_op_abandon : (connection_id -> ldap_message -> unit) option;
  bi_op_extended : (connection_id -> ldap_message -> ldap_message) option;
  bi_init : (unit -> unit) option;
  bi_close : (unit -> unit) option;
}

type log_level =
    [ `GENERAL
    | `CONNECTION
    | `OPERATIONS
    | `ERROR
    | `TRACE ]

type msgid = int
type opcnt = int
type pending_operations = (unit -> unit) list

type server_info = {
  si_listening_socket: file_descr;
  si_client_sockets: (file_descr, connection_id * opcnt * pending_operations * readbyte) Hashtbl.t;
  si_backend: backendInfo;
  si_log: (log_level -> string -> unit);
  mutable si_current_connection_id: int;
}

let allocate_connection_id si =
  if si.si_current_connection_id < max_int then
    (si.si_current_connection_id <- si.si_current_connection_id + 1;
     si.si_current_connection_id)
  else
    (si.si_current_connection_id <- 1;1)

let log_result conn_id op_nr si msg =
  let log_search_result {result_code=err;error_message=text} =
    si.si_log `OPERATIONS
      (sprintf "conn=%d op=%d SEARCH RESULT tag=0 err=%d nentries=0 text=%s"
         conn_id op_nr (Ldap_protocol.encode_resultcode err) text)
  in
  let log_normal_result {result_code=err;error_message=text} =
    si.si_log `OPERATIONS
      (sprintf "conn=%d op=%d RESULT tag=0 err=%d text=%s"
         conn_id op_nr (Ldap_protocol.encode_resultcode err) text)
  in
    match msg.protocolOp with
        Bind_response {bind_result=result}
      | Modify_response result
      | Add_response result
      | Delete_response result
      | Modify_dn_response result
      | Compare_response result -> log_normal_result result
      | Search_result_done result -> log_search_result result
      | _ -> ()

let send_message si conn_id op_nr fd msg =
  let e_msg = encode_ldapmessage msg in
  let e_msg = Bytes.of_string e_msg in
  let len = Bytes.length e_msg in
  let written = ref 0 in
    try
      while !written < len
      do
        written := ((write fd e_msg
                       !written (len - !written)) +
                      !written)
      done;
      log_result conn_id op_nr si msg
    with Unix_error (_, _, _) ->
      (try close fd with _ -> ());
      raise (Server_error "data cannot be written")

let keys h = Hashtbl.fold (fun k v l -> k :: l) h []

let init ?(log=(fun _ _ -> ())) ?(port=389) bi =
  let s =
    let s = socket PF_INET SOCK_STREAM 0 in
      setsockopt s SO_REUSEADDR true;
      bind s (ADDR_INET (inet_addr_any, port));
      listen s 500;
      s
  in
    (match bi.bi_init with
         Some f -> f ()
       | None -> ());
    {si_listening_socket=s;
     si_client_sockets=Hashtbl.create 10;
     si_current_connection_id=0;
     si_log=log;
     si_backend=bi}

let shutdown si =
  (match si.si_backend.bi_close with
       Some f -> f ()
     | None -> ());
  close si.si_listening_socket;
  List.iter (fun fd -> close fd) (keys si.si_client_sockets);
  Hashtbl.clear si.si_client_sockets;
  si.si_log `GENERAL "stopped."

let dispatch_request si conn_id op_nr rb fd =
  let bi = si.si_backend in
  let not_imp msg op =
    {messageID=msg.messageID;
     protocolOp=op;
     controls=None}
  in
  let not_implemented = {result_code=`OTHER;
                         matched_dn="";
                         error_message="Not Implemented";
                         ldap_referral=None}
  in
  let message = decode_ldapmessage rb in
    match message with
        {protocolOp=Bind_request {bind_name=dn;bind_authentication=auth}} ->
          si.si_log `OPERATIONS
            (sprintf "conn=%d op=%d BIND dn=\"%s\" method=128" conn_id op_nr dn);
          si.si_log `OPERATIONS
            (sprintf "conn=%d op=%d BIND dn=\"%s\" mech=%s ssf=0" conn_id op_nr dn
               (match auth with
                    Simple _ -> "SIMPLE"
                  | Sasl _ -> "SASL"));
          (match bi.bi_op_bind with
               Some f ->
                 (fun () ->
                    send_message si conn_id op_nr fd (f conn_id message);
                    raise Finished)
             | None -> (fun () -> send_message si conn_id op_nr fd
                          (not_imp message (Bind_response
                                              {bind_result=not_implemented;
                                               bind_serverSaslCredentials=None}));
                          raise Finished))
      | {protocolOp=Unbind_request} ->
          si.si_log `OPERATIONS
            (sprintf "conn=%d op=%d UNBIND" conn_id op_nr);
          (match bi.bi_op_unbind with
               Some f -> (fun () -> f conn_id message;raise Finished)
             | None -> (fun () -> raise Finished))
      | {protocolOp=(Search_request
                       {baseObject=base;
                        scope=scope;
                        derefAliases=deref;
                        sizeLimit=sizelimit;
                        timeLimit=timelimit;
                        typesOnly=attrsonly;
                        filter=filter;
                        s_attributes=attrs})} ->
          si.si_log `OPERATIONS
            (sprintf "conn=%d op=%d SRCH base=\"%s\" scope=%d deref=%d filter=\"%s\""
               conn_id op_nr base
               (match scope with
                    `BASE -> 0
                  | `ONELEVEL -> 1
                  | `SUBTREE -> 2)
               (match deref with
                    `NEVERDEREFALIASES -> 0
                  | `DEREFINSEARCHING -> 1
                  | `DEREFFINDINGBASE -> 2
                  | `DEREFALWAYS -> 3)
               (Ldap_filter.to_string filter));
          (match attrs with
               [] -> ()
             | lst -> si.si_log `OPERATIONS
                 (sprintf "conn=%d op=%d SRCH attr=%s" conn_id op_nr
                    (List.fold_left
                       (fun s attr -> if s = "" then attr else (attr ^ " " ^ s))
                       "" lst)));
          (match bi.bi_op_search with
               Some f ->
                 let get_srch_result = f conn_id message in
                 (fun () ->
                    let msg = get_srch_result () in
                    send_message si conn_id op_nr fd msg;
                    match msg.protocolOp with
                        Search_result_done _ -> raise Finished
                      | _ -> ())
             | None -> (fun () -> send_message si conn_id op_nr fd
                          (not_imp message (Search_result_done not_implemented));
                          raise Finished))
      | {protocolOp=Modify_request {mod_dn=modify;modification=modlst}} ->
          si.si_log `OPERATIONS
            (sprintf "conn=%d op=%d MOD dn=\"%s\"" conn_id op_nr modify);
          si.si_log `OPERATIONS
            (sprintf "conn=%d op=%d MOD attr=\"%s\"" conn_id op_nr
               (List.fold_left
                  (fun s attr ->
                     if s = "" then
                       attr.mod_value.attr_type
                     else
                       (attr.mod_value.attr_type ^ " " ^ s))
                  "" modlst));
          (match bi.bi_op_modify with
               Some f -> (fun () ->
                            send_message si conn_id op_nr fd (f conn_id message);
                            raise Finished)
             | None -> (fun () -> send_message si conn_id op_nr fd
                          (not_imp message (Modify_response not_implemented));
                          raise Finished))
      | {protocolOp=Add_request {sr_dn=dn}} ->
          si.si_log `OPERATIONS (sprintf "conn=%d op=%d ADD dn=\"%s\"" conn_id op_nr dn);
          (match bi.bi_op_add with
               Some f -> (fun () ->
                            send_message si conn_id op_nr fd (f conn_id message);
                            raise Finished)
             | None -> (fun () -> send_message si conn_id op_nr fd
                          (not_imp message (Add_response not_implemented));
                          raise Finished))
      | {protocolOp=Delete_request dn} ->
          si.si_log `OPERATIONS (sprintf "conn=%d op=%d DEL dn=\"%s\"" conn_id op_nr dn);
          (match bi.bi_op_delete with
               Some f -> (fun () ->
                            send_message si conn_id op_nr fd (f conn_id message);
                            raise Finished)
             | None -> (fun () -> send_message si conn_id op_nr fd
                          (not_imp message (Delete_response not_implemented));
                          raise Finished))
      | {protocolOp=Modify_dn_request {modn_dn=dn}} ->
          si.si_log `OPERATIONS (sprintf "conn=%d op=%d MODRDN dn=\"%s\"" conn_id op_nr dn);
          (match bi.bi_op_modrdn with
               Some f -> (fun () ->
                            send_message si conn_id op_nr fd (f conn_id message);
                            raise Finished)
             | None -> (fun () -> send_message si conn_id op_nr fd
                          (not_imp message (Modify_dn_response not_implemented));
                          raise Finished))
      | {protocolOp=Compare_request {cmp_dn=dn;cmp_ava=ava}} ->
          si.si_log `OPERATIONS
            (sprintf "conn=%d op=%d CMP dn=\"%s\" attr=\"%s\""
               conn_id op_nr dn ava.attributeDesc);
          (match bi.bi_op_compare with
               Some f -> (fun () ->
                            send_message si conn_id op_nr fd (f conn_id message);
                            raise Finished)
             | None -> (fun () -> send_message si conn_id op_nr fd
                          (not_imp message (Compare_response not_implemented));
                          raise Finished))
      | {protocolOp=Abandon_request msgid} ->
          si.si_log `OPERATIONS (sprintf "conn=%d op=%d ABANDON msgid=%ld" conn_id op_nr msgid);
          (match bi.bi_op_abandon with
               Some f -> (fun () -> f conn_id message;raise Finished)
             | None -> (fun () -> raise Finished))
      | {protocolOp=Extended_request _} ->
          (match bi.bi_op_extended with
               Some f -> (fun () ->
                            send_message si conn_id op_nr fd (f conn_id message);
                            raise Finished)
             | None -> (fun () -> send_message si conn_id op_nr fd
                          (not_imp message
                             (Extended_response
                                {ext_result=not_implemented;
                                 ext_responseName=None;
                                 ext_response=None}));
                          raise Finished))
      | _ -> raise (Server_error "invalid operation")

let string_of_sockaddr sockaddr =
  match sockaddr with
      ADDR_UNIX addr -> addr
    | ADDR_INET (ip, port) ->
        (sprintf "%s:%d" (string_of_inet_addr ip) port)

let run si =
  let pending_writes si = (* do we have data to write? *)
    Hashtbl.fold
      (fun k (_, _, ops_pending, _) pending ->
         match ops_pending with
             [] -> pending
           | _ -> k :: pending)
      si.si_client_sockets []
  in
  let process_read reading writing excond (fd:file_descr) =
    if Hashtbl.mem si.si_client_sockets fd then
      (* an existing client has requested a new operation *)
      let (conn_id, op_nr, pending_ops, rb) = Hashtbl.find si.si_client_sockets fd in
        try
          try
            Hashtbl.replace
              si.si_client_sockets
              fd
              (conn_id,
               (op_nr + 1),
               (dispatch_request si conn_id op_nr rb fd) :: pending_ops,
               rb)
          with LDAP_Decoder e | Decoding_error e -> (* handle protocol errors *)
            send_message si conn_id 0 fd (* send a notice of disconnection *)
              {messageID=0l;
               protocolOp=Extended_response
                  {ext_result={result_code=`PROTOCOL_ERROR;
                               matched_dn="";
                               error_message=e;
                               ldap_referral=None};
                   ext_responseName=(Some "1.3.6.1.4.1.1466.20036");
                   ext_response=None};
               controls=None};
            raise (Readbyte_error Transport_error) (* close the connection *)
        with Readbyte_error Transport_error ->
          (match si.si_backend.bi_op_unbind with
               Some f -> f conn_id {messageID=0l;protocolOp=Unbind_request;controls=None}
             | None -> ());
          (* remove the client from our table of clients, and
             from the list of readable/writable fds, that way we
             don't try to do a write to them, even though we may
             have pending writes *)
          Hashtbl.remove si.si_client_sockets fd;
          reading := List.filter ((<>) fd) !reading;
          writing := List.filter ((<>) fd) !writing;
          excond := List.filter ((<>) fd) !excond;
          (try close fd with _ -> ());
          si.si_log `CONNECTION (sprintf "conn=%d fd=0 closed" conn_id)
    else (* a new connection has come in, accept it *)
      let (newfd, sockaddr) = accept fd in
      let rb = readbyte_of_fd newfd in
      let connid = allocate_connection_id si in
        Hashtbl.add si.si_client_sockets newfd (connid, 0, [], rb);
        si.si_log `CONNECTION
          (sprintf "conn=%d fd=0 ACCEPT from IP=%s (IP=%s)"
             connid
             (string_of_sockaddr sockaddr)
             (string_of_sockaddr (getsockname fd)))
  in
  let process_write reading writing excond (fd: file_descr) =
    if Hashtbl.mem si.si_client_sockets fd then
      let (conn_id, op_nr, pending_ops, rb) = Hashtbl.find si.si_client_sockets fd in
        try
          match pending_ops with
              [] -> ()
            | hd :: tl ->
                try hd () with Finished ->
                  Hashtbl.replace si.si_client_sockets fd (conn_id, op_nr, tl, rb)
        with Server_error "data cannot be written" ->
          (match si.si_backend.bi_op_unbind with
               Some f -> f conn_id {messageID=0l;protocolOp=Unbind_request;controls=None}
             | None -> ());
          Hashtbl.remove si.si_client_sockets fd;
          reading := List.filter ((<>) fd) !reading;
          writing := List.filter ((<>) fd) !writing;
          excond := List.filter ((<>) fd) !excond;
          si.si_log `CONNECTION (sprintf "conn=%d fd=0 closed" conn_id)
    else raise (Server_error "socket to write to not found")
  in
    si.si_log `GENERAL "starting";
    while true
    do
      let fds = keys si.si_client_sockets in
      let reading = ref []
      and writing = ref []
      and excond = ref [] in
      let (rd, wr, ex) =
        select (si.si_listening_socket :: fds)
          (pending_writes si) (* nothing to write? don't bother *)
          fds (-1.0)
      in
        reading := rd;writing := wr;excond := ex;

        (* service connections which are ready to be read *)
        List.iter (process_read reading writing excond) !reading;

        (* service connections which are ready to be written to *)
        List.iter (process_write reading writing excond) !writing;

        (* Process out of band data *)
        List.iter (process_read reading writing excond) !excond
    done
