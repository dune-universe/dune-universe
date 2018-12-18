(*
  Copyright (C) iNuron - info@openvstorage.com
  This file is part of Open vStorage. For license information, see <LICENSE.txt>
*)

open Cryptokit
open Kinetic_util
open Kinetic_network
open Kinetic_types


let calculate_hmac secret msg =
  let msg_len = Bytes.length msg in
  let sx0 = Bytes.create 4 in
  _encode_fixed32 sx0 0 msg_len;
  let h = Cryptokit.MAC.hmac_sha1 secret in
  let () = h # add_substring sx0 0 4 in
  let () = h # add_substring msg 0 msg_len in
  h # result |> Bytes.of_string



let _assert_type (command:command) typ =
  let header  = unwrap_option "header" command.header in
  let htyp    = unwrap_option "message type" header.message_type in
  assert (htyp = typ)

let _get_status (command: command) =
  let status = unwrap_option "status" command.status in
  status

let _get_status_code (status:command_status) =
  let code = unwrap_option "status.code" status.code in
  code

let _get_status_message (status:command_status) =
  let msg = unwrap_option "status.message" status.status_message in
  msg

let _get_detailed_status_message =
  let none_bytes = Bytes.of_string "None" in
  fun (status:command_status) -> get_option none_bytes status.detailed_message

let status_code2i = function
  | Invalid_status_code     -> -1
  | Not_attempted           ->  0 (* p2p *)
  | Success                 ->  1
  | Hmac_failure            ->  2
  | Not_authorized          ->  3
  | Version_failure         ->  4
  | Internal_error          ->  5
  | Header_required         ->  6
  | Not_found               ->  7
  | Version_mismatch        ->  8
  | Service_busy            ->  9
  | Expired                 -> 10
  | Data_error              -> 11
  | Perm_data_error         -> 12
  | Remote_connection_error -> 13
  | No_space                -> 14
  | No_such_hmac_algorithm  -> 15
  | Invalid_request         -> 16
  | Nested_operation_errors -> 17
  | Device_locked           -> 18
  | Device_already_unlocked -> 19
  | Connection_terminated   -> 20
  | Invalid_batch           -> 21 (* 3.0.6 *)
  | _ -> 42

let _parse_command (m:message) =
  let command_bytes = unwrap_option "command_bytes" m.command_bytes in
  let command = Kinetic_pb.decode_command (Pbrt.Decoder.of_bytes command_bytes) in
  command

module Error = Kinetic_error.Error

let _assert_success (command:command) =
  let code = _get_status command |> _get_status_code in
  assert (code = Success)

let message_type2s = function
  | Invalid_message_type -> "invalide_message_type"
  | Get -> "get"
  | Get_response -> "get_response"
  | Put -> "put"
  | Put_response -> "put_response"
  | Delete -> "delete"
  | Delete_response -> "delete_response"
  | Start_batch_response -> "start_batch_response"
  | End_batch_response -> "end_batch_response"
  | Flushalldata -> "flushalldata"
  | Flushalldata_response -> "flushalldata_response"
  | Getlog -> "getlog"
  | Getlog_response -> "getlog_response"
  | _ -> "TODO: message type2s"

(*
let auth_type2s = function
   | `invalid_auth_type -> "invalid_auth_type"
   | `hmacauth -> "hmacauth"
   | `pinauth -> "pinauth"
   | `unsolicitedstatus -> "unsolicitedstatus"
 *)

let status_code2s = function
  | Invalid_status_code -> "invalid_status_code"
  | Not_attempted -> "not_attempted"
  | Success -> "success"
  | Hmac_failure -> "hmac_failure"
  | Not_authorized -> "not_authorized"
  | Version_failure -> "version_failure"
  | Internal_error -> "internal_error"
  | Header_required -> "header_required"
  | Not_found -> "not_found"
  | Version_mismatch -> "version_mismatch"
  | Service_busy -> "service_busy"
  | Expired -> "expired"
  | Data_error -> "data_error"
  | Perm_data_error -> "perm_data_error"
  | Remote_connection_error ->"remove_connection_error"
  | No_space -> "no_space"
  | No_such_hmac_algorithm -> "no_such_hmac_algorithm"
  | Invalid_request -> "invalid_request"
  | _ -> "TODO: status_code2s"



let _get_message_type (command:command) =
  let header = unwrap_option "header" command.header in
  unwrap_option "message_type" header.message_type


let _get_message_auth_type msg = unwrap_option "auth_type" msg.auth_type

let _assert_both m command typ code =
  let auth_type = _get_message_auth_type m in
  match auth_type with
  | Hmacauth | Pinauth ->
     begin
       let header = unwrap_option "header" command.header in
       let htyp = unwrap_option "message type" header.message_type in
       if htyp = typ
       then
         begin
           let status = _get_status command in
           let ccode = _get_status_code status in
           if ccode = code
           then Lwt_result.return ()
           else
             begin
               let () = Printf.printf "ccode:%s\n%!" (status_code2s ccode) in
               let sm = _get_status_message status in
               let rci = status_code2i ccode in
               Lwt_result.fail (Error.KineticError(rci, sm))
             end
         end
       else
         let msg =
           htyp
           |> message_type2s
           |> Printf.sprintf "unexpected type: %s"
         in
         let e = Error.Generic(__FILE__,__LINE__, msg) in
         Lwt_result.fail e
     end
  | Unsolicitedstatus ->
     let status = _get_status command in
     let ccode = _get_status_code status in
     let sm = _get_status_message status in
     let rci = status_code2i ccode in
     Lwt_result.fail (Error.KineticError(rci, sm))
  | Invalid_auth_type ->
     let e = Error.Assert "invalid_auth_type" in
     Lwt_result.fail e



let maybe_verify_msg (m:message) =
  match m.auth_type with
  | Some Unsolicitedstatus -> ()
  | _ -> failwith "todo:verify_msg"


let verify_status (status:command_status) =
  let code = unwrap_option "status.code" status.code in
  match code with
  | Success -> ()
  | _ -> failwith "todo: other status"



let verify_cluster_version (header:command_header) my_cluster_version =
  let cluster_version = get_option 0L header.cluster_version in
  assert (my_cluster_version = cluster_version);
  ()

let verify_limits log = ()

open Lwt


let _get_sequence (command : command) =
  let header = unwrap_option "header" command.header in
  let seq = unwrap_option "sequence" header.sequence in
  seq

let _get_ack_sequence (command:command) =
  let header = unwrap_option "header" command.header in
  let ack_seq = unwrap_option "ack_sequence" header.ack_sequence in
  ack_seq


module Config = Kinetic_config.Config
module Session = Kinetic_session.Session
module Tag = Kinetic_tag.Tag

type priority = Kinetic_types.command_priority =
  | Normal
  | Lowest
  | Lower
  | Low
  | Lowernormal
  | Highernormal
  | High
  | Higher
  | Highest

              
include Kinetic_integration

module Batch(I:INTEGRATION) =
struct

  type t = { socket : I.socket;
             batch_id : int32;
             session : Session.t;
             mutable count : int;
           }

  let make session (socket:I.socket) batch_id =
    let batch = { socket; batch_id; session; count = 0} in
    batch


  let inc_count t = t.count <- t.count + 1

end

  
module Make(I:INTEGRATION) = struct

  module Entry = struct
    type t = {
        key:key;
        db_version:version;
        new_version : version;
        vt: (I.value slice * Tag.t) option;
      }

    let make ~key ~db_version ~new_version vt = { key; db_version; new_version; vt }


    let show e =
      let vt2s = show_option
                   (show_pair
                      (show_tuple3 I.show string_of_int string_of_int)
                      Tag.show)
      in
      Printf.sprintf "{ key=%S; db_version=%s; new_version=%s; vo=%s }"
        (e.key |> Bytes.to_string)
        (so2hs e.db_version)
        (so2hs e.new_version)
        (vt2s e.vt)
  end



  type 'a result = ('a, Error.t) Lwt_result.t
  module B = Batch(I)

    type session = Session.t

    let get_connection_id (session:session) =
      let open Session in session.connection_id


    type batch = B.t
    let get_batch_id (batch:batch) =
      let open B in batch.batch_id
    type synchronization =
      |WRITETHROUGH
      |WRITEBACK
      |FLUSH
    

    type closer = unit -> unit Lwt.t

    type client = {
        session : session ;
        socket: I.socket;
        closer : closer;
        mutable closed : bool;
      }

    let make_sha1  (v_buf, v_off,v_len) = I.make_sha1  v_buf v_off v_len
    let make_crc32 (v_buf, v_off,v_len) = I.make_crc32 v_buf v_off v_len
                         
    let get_config (session:Session.t) =
      let open Session in
      session.config

    let handshake secret cluster_version ?(trace = false) ?(timeout=10.0) ?max_operation_count_per_batch socket  =
      network_receive_generic ~timeout I.create I.read I.read_bytes socket I.show_socket trace >>=? fun (m,vo,_) ->
      let () = maybe_verify_msg m in
      let command = _parse_command m in
      let status = unwrap_option "command.status" command.status in
      let () = verify_status status in
      let header = unwrap_option "command.header" command.header in
      
      let connection_id = unwrap_option "header.connection_id" header.connection_id in
      Lwt_log.debug_f ~section "connection_id:%Li" connection_id >>= fun () ->
      Lwt_log.debug_f "sequence:%s" (i64o2s header.sequence) >>= fun () ->
      Lwt_log.debug_f "ack_sequence:%s" (i64o2s header.ack_sequence) >>= fun () ->
      let () = verify_cluster_version header cluster_version in
      
      let body = unwrap_option "command.body" command.body in
      
      let log = unwrap_option "body.get_log" body.get_log in
      (*
         self.config = cmd.body.getLog.configuration
         self.limits = cmd.body.getLog.limits
       *)

      
      let () = verify_limits log in
      let cfg = unwrap_option "configuration" log.configuration in
      let wwn = unwrap_option "world_wide_name" cfg.world_wide_name in
      let interfaces = cfg.interface in
      let ipv4_addresses =
        List.fold_left
          (fun acc interface ->
            
            let ip4bin = unwrap_option "ipv4_address" interface.ipv4_address in
            if ip4bin = Bytes.empty (* this nic has no connection, skip it *)
            then acc
            else ip4bin :: acc
          ) []
          interfaces
        |> List.rev
      in
      let vendor = unwrap_option "vendor" cfg.vendor in
      let model = unwrap_option "model" cfg.model in
      let serial_number = unwrap_option
                            "serial_number"
                            cfg.serial_number in
      let version = unwrap_option "version" cfg.version in
      let limits = unwrap_option "limits" log.limits in
      
      let int_of k o =
        let m_k_s32 = unwrap_option k o in
        Int32.to_int m_k_s32
      in
      let max_key_size     = int_of "max_key_size" limits.max_key_size
      and max_value_size   = int_of "max_value_size" limits.max_value_size
      and max_version_size = int_of "max_version_size" limits.max_version_size
      and max_tag_size     = int_of "max_tag_size" limits.max_tag_size
      and max_connections  = int_of "max_connections" limits.max_connections
      and max_outstanding_read_requests =
        int_of "max_outstanding_read_requests" limits.max_outstanding_read_requests
      and max_outstanding_write_requests =
        int_of "max_outstanding_write_requests" limits.max_outstanding_write_requests
      and max_message_size = int_of "max_message_size" limits.max_message_size
      and max_key_range_count =
        int_of "max_key_range_count" limits.max_key_range_count
      (* and max_operation_count_per_batch =
        int_of "max_operation_count_per_batch" limits.max_operation_count_per_batch

      and max_batch_count_per_device =
        int_of "max_batch_count_per_device" limits.max_batch_count_per_device
       *)
      and max_batch_size        = map_option Int32.to_int limits.max_batch_size
      and max_deletes_per_batch = map_option Int32.to_int limits.max_deletes_per_batch
      in
      let max_operation_count_per_batch =
        if String.length version >= 8
           && String.sub version 0 8 = "07.00.03"
        then Some 15
        else max_operation_count_per_batch
      in
      let config =
        Config.make ~vendor
                    ~world_wide_name:wwn
                    ~model
                    ~serial_number
                    ~version
                    ~ipv4_addresses
                    ~max_key_size
                    ~max_value_size
                    ~max_version_size
                    ~max_tag_size
                    ~max_connections
                    ~max_outstanding_read_requests
                    ~max_outstanding_write_requests
                    ~max_message_size
                    ~max_key_range_count
                    ~max_operation_count_per_batch
                    ~max_batch_size
                    ~max_deletes_per_batch
                    ~timeout
                    (* ~max_batch_count_per_device *)
      in
      Lwt_log.debug_f "config=%s" (Config.show config) >>= fun () ->
      let session =
        let open Session in {
          cluster_version;
          identity = 1L;
          sequence = 1L;
          secret;
          connection_id;
          batch_id = 1l;
          config ;
          trace;
          in_batch = false;
        }
      in
      Lwt_result.return session


    let _assert_response (m:message) (client:client) =
      
      match m.auth_type with
      | Some Unsolicitedstatus ->
         begin
           let command = _parse_command m in
           let status = _get_status command in
           let ccode = _get_status_code status in
           let sm = _get_status_message status in
           let rci = status_code2i ccode in
           let e = Error.KineticError(rci,sm) in
           client.closed <- true;
           client.closer () >>= fun () ->
           Lwt_result.fail e
         end
      | _ -> Lwt_result.return ()


    let make_header ~session ~message_type ?timeout ?priority ?batch_id ?ack_sequence () =
      let open Session in
      {
        cluster_version = Some session.cluster_version;
        connection_id = Some session.connection_id;
        sequence = Some session.sequence;
        timeout;
        priority;
        message_type = Some message_type;
        batch_id;
        ack_sequence;
        early_exit = None;
        time_quanta = None;
      }

    let make_body ?key_value ?batch ?get_log ?range ?pin_op ?setup () =
      {
        key_value;
        batch;
        get_log;
        range;
        pin_op;
        setup;
        
        p2p_operation = None;
        security = None;
        
        power = None;
      }

    let make_command header body =
      {
        header = Some header;
        body = Some body;
        status = None;
      }
      
    let make_command_bytes command =
      let encoder = Pbrt.Encoder.create () in
      Kinetic_pb.encode_command command encoder;
      let command_bytes = Pbrt.Encoder.to_bytes encoder in
      command_bytes

    let make_message_bytes message =
      let encoder = Pbrt.Encoder.create() in
      Kinetic_pb.encode_message message encoder;
      let proto_raw = Pbrt.Encoder.to_bytes encoder in
      proto_raw
      
    let make_serialized_msg ?timeout ?priority session message_type ~body =
      let open Session in

      let header = make_header ~session ~message_type ?timeout ?priority () in

      let command = make_command header body in
      
      let command_bytes = make_command_bytes command in
      let hmac = calculate_hmac session.secret command_bytes in
      let hmac_auth = { 
          identity = Some session.identity;
          hmac = Some hmac;
        }
      in

      let m = {
          command_bytes = Some command_bytes;
          auth_type = Some Hmacauth;
          hmac_auth = Some hmac_auth;
          pin_auth = None;
        }
      in
      make_message_bytes m

  let make_pin_auth_serialized_msg session (pin:bytes) message_type ~body =

    let open Session in

    let header = make_header ~session ~message_type () in
    let command = {
        header = Some header;
        body = Some body;
        status = None;
      }
    in
    

    let command_bytes = make_command_bytes command in
    let pin_auth = {
        pin = Some pin
      }
    in
    let m = {
        command_bytes = Some command_bytes;
        auth_type = Some Pinauth;
        pin_auth = Some pin_auth;
        hmac_auth = None;
      }
    in
    make_message_bytes m

  let body_with_attributes
        ?pin_op
        ~ko
        ~db_version
        ~new_version
        ~forced
        ~synchronization
        ~maybe_tag
        ()
    =

    let translate = function
      | WRITETHROUGH -> Writethrough
      | WRITEBACK -> Writeback
      | FLUSH     -> Flush
    in
    let sync = map_option translate synchronization in
    let algorithm, tag =
      match maybe_tag with
      | None -> None, None
      | Some tag ->
         begin
           match tag with
           | Tag.Invalid h -> Some Invalid_algorithm, Some h
           | Tag.Sha1 h    -> Some Sha1, Some h
           | Tag.Crc32 h   ->
              let s = Bytes.create 4 in
              _encode_fixed32 s 0 (Int32.to_int h);
              Some Crc32, Some s
         end
    in
    let key_value = {
        key = ko;
        force = forced;
        db_version;
        new_version;
        synchronization = sync;
        metadata_only = None;
        tag;
        algorithm;
      }
    in
    make_body ~key_value ?pin_op()

  let make_delete_forced ?timeout ?priority client key =
    let body =
      body_with_attributes
        ~ko:(Some key)
        ~db_version:None
        ~new_version:None
        ~forced:(Some true)
        ~maybe_tag:None
        ~synchronization:(Some WRITEBACK) ()
    in
    make_serialized_msg ?timeout ?priority client.session Delete ~body

  let make_put
        ?timeout ?priority client
        key value
        ~db_version ~new_version
        ~forced ~synchronization
        ~tag
    =
    let body =
      body_with_attributes
        ~ko:(Some key)
        ~db_version
        ~new_version
        ~forced
        ~synchronization
        ~maybe_tag:tag ()
    in
    make_serialized_msg ?timeout ?priority client.session Put ~body

  

  let make_flush ?timeout ?priority session =
    let body = make_body () in
    make_serialized_msg ?timeout ?priority session Flushalldata ~body

  let make_batch_message
        ?timeout
        ?priority
        ~body
        session message_type batch_id =
    
    let open Session in
    
    let header = make_header ~session ~message_type ?timeout ?priority ~batch_id () in
    let command = make_command header body in

    

    let command_bytes = make_command_bytes command in
    let hmac = calculate_hmac session.secret command_bytes in
    let hmac_auth = {
        identity = Some session.identity;
        hmac = Some hmac;
      }
    in
    let m = {
        command_bytes = Some command_bytes;
        auth_type = Some Hmacauth;
        hmac_auth = Some hmac_auth;
        pin_auth = None;
      }
    in
    make_message_bytes m

  let make_start_batch ?timeout ?priority session batch_id =
    make_batch_message ?timeout ?priority session Start_batch batch_id

  let make_end_batch (b:B.t) =
    let open B in
    let count = b.count in
    let (batch:command_batch) = {
        count = Some (Int32.of_int count);
        sequence = [];
        failed_sequence = None;
      }
    in
    let body = make_body ~batch () in
    make_batch_message ~body b.session End_batch b.batch_id 


  let tracing (session:Session.t) t =
    session.Session.trace <- t

  let _call client msg vo =
    let socket = client.socket in
    let session = client.session in
    let config = session.config in
    let trace = client.session.trace in
    let timeout = config.timeout in
    network_send_generic             I.write I.write_bytes socket msg vo I.show_socket trace >>= fun () ->
    network_receive_generic I.create I.read  I.read_bytes  socket        I.show_socket trace ~timeout

  let get_session t  = t.session

  let _assert_value_size session value_size =
    let cfg = get_config session in
    let max_value_size = cfg.Config.max_value_size in
    if value_size > max_value_size
    then Lwt.fail_with (Printf.sprintf "value_size:%i > max_value_size:%i" value_size max_value_size)
    else Lwt.return_unit

  let _assert_open (client:client) =
    if client.closed
    then Lwt.fail_with (Printf.sprintf "Generic(%S,%i,%S)" __FILE__ __LINE__ "client is closed")
    else Lwt.return_unit

  let _assert_no_batch (client:client) =
    if client.session.in_batch
    then Lwt.fail_with (Printf.sprintf "Generic(%S,%i,%S)" __FILE__ __LINE__ "client has open batch")
    else Lwt.return_unit

  let put
        ?timeout
        ?priority
        (client:client) k
        ((v_buf, v_off, v_len) as v_slice)
        ~db_version ~new_version
        ~forced
        ~synchronization
        ~tag
    =
    _assert_open client >>= fun () ->
    _assert_no_batch client >>= fun () ->
    _assert_value_size client.session v_len >>= fun () ->

    let msg =
      make_put
        ?timeout ?priority client
        k v_slice
        ~db_version ~new_version
        ~forced ~synchronization
        ~tag
    in
    _call client msg (Some v_slice) >>=? fun (r,vo,_) ->
    assert (vo = None);
    _assert_response r client >>=? fun () ->
    let command = _parse_command r in
    let () = Session.incr_sequence client.session in
    _assert_both r command Put_response Success

  let delete_forced ?timeout ?priority (client:client) k =
    _assert_open client >>= fun () ->
    _assert_no_batch client >>= fun () ->
    let msg = make_delete_forced ?timeout ?priority client k in
    _call client msg None >>=? fun (r, vo, _ ) ->
    _assert_response r client >>=? fun () ->
    assert (vo = None);
    let command = _parse_command r in
    let () = Session.incr_sequence client.session in
    _assert_type command Delete_response;
    _assert_success command;
    Lwt_result.return ()



  let make_get ?timeout ?priority session key =
    let body = body_with_attributes ~ko:(Some key)
                 ~db_version:None
                 ~new_version:None
                 ~forced:None
                 ~synchronization:None
                 ~maybe_tag:None ()
    in
    make_serialized_msg ?timeout ?priority session Get ~body

  let get ?timeout ?priority client k =
    _assert_open client >>= fun () ->
    _assert_no_batch client >>= fun () ->
    let msg = make_get ?timeout ?priority client.session k in
    _call client msg None >>=? fun (r,vo, proto_raw) ->
    _assert_response r client >>=? fun () ->
    let command = _parse_command r in

    (* _assert_type command  `get_response;*)

    let status = _get_status command in

    let code = _get_status_code status in
    Lwt_log.debug_f ~section "code=%i" (status_code2i code) >>= fun () ->
    let () = Session.incr_sequence client.session in
    match code with
    | Not_found ->
       Lwt_log.debug_f "`not_found" >>= fun () ->
       Lwt_result.return None
    | Success    ->
       Lwt_log.debug_f "Success" >>= fun () ->
       begin
         let version =
           let body = unwrap_option "body" command.body in
           let kv = unwrap_option "kv" body.key_value in
           let db_version = kv.db_version in
           db_version
         in
         let v = match vo with
           | None -> I.create 0
           | Some v -> v
         in
         let result = Some (v, version) in
         Lwt_result.return result
       end
    | x ->
       let code = status_code2i x in
       Lwt_log.info_f ~section "code=%i" code >>= fun () ->
       let sm = _get_status_message status in
       let e = Error.KineticError(code, sm) in
       Lwt_result.fail e

  type log_type =
    | CAPACITIES

  let translate_log_type = function
    | CAPACITIES -> Capacities

  let make_getlog ?timeout ?priority session capacities =
    let get_log = {
        types = List.map translate_log_type capacities;
        utilizations = [];
        temperatures = [];

        capacity = None;
        configuration = None;
        statistics = [];
        messages = None;
        limits = None;
        device = None;
      }
    in
    let body = make_body ~get_log() in
    make_serialized_msg ?timeout ?priority session Getlog ~body

  let get_capacities ?timeout ?priority client =
    _assert_open client >>= fun () ->
    _assert_no_batch client >>= fun () ->
    let msg = make_getlog ?timeout ?priority client.session [CAPACITIES] in
    _call client msg None >>=? fun (m,vo, proto_raw) ->
    _assert_response m client >>=? fun () ->
    let command = _parse_command m in
    _assert_type command Getlog_response;
    let status = _get_status command in
    let code = _get_status_code status in
    let () = Session.incr_sequence client.session in
    match code with
    | Success ->
       let body = unwrap_option "body" command.body in
       let getlog = unwrap_option "getlog" body.get_log in
       let capacity = unwrap_option "capacity" getlog.capacity in
       let nominal = unwrap_option "nominal_capacity_in_bytes" capacity.nominal_capacity_in_bytes in
       let portion_full = unwrap_option "portion_full" capacity.portion_full in
       Lwt_result.return (nominal, portion_full)

    | x ->
       let code = status_code2i x in
       let sm = _get_status_message status in
       let e = Error.KineticError(code,sm) in
       Lwt_result.fail e

  let body_with_kr start_key sinc maybe_end_key reverse_results max_results =
    let end_key, end_key_inclusive =
      match maybe_end_key with
      | None -> None, None
      | Some (end_key, einc) -> Some end_key, Some einc
    in
    let max_returned = Some (Int32.of_int max_results) in
    let range = {
        reverse = Some reverse_results;
        start_key = Some start_key;
        start_key_inclusive = Some sinc;
        end_key;
        end_key_inclusive;
        max_returned;
        keys = [];
      }
    in
    make_body ~range ()

  let make_get_key_range
        ?timeout ?priority session
        start_key sinc
        maybe_end_key
        reverse_results
        max_results =
      let body = body_with_kr
                   start_key sinc
                   maybe_end_key
                   reverse_results
                   max_results
      in
      make_serialized_msg ?timeout ?priority session Getkeyrange ~body

  let get_key_range_result (command : command) =
    match command.body with
    | None -> []
    | Some body ->
      let range   = unwrap_option "range" body.range in
      
      let (keys:key list) = range.keys in
      keys


  let get_key_range
        ?timeout ?priority client
        (start_key:key) sinc
        (maybe_end_key: (key * bool) option)
        (reverse_results:bool)
        (max_results:int) =
    _assert_open client >>= fun () ->
    _assert_no_batch client >>= fun () ->
    let msg = make_get_key_range
                ?timeout ?priority client.session
                start_key sinc
                maybe_end_key reverse_results
                max_results
    in
    _call client msg None >>=? fun (r,vo, _) ->
    _assert_response r client >>=? fun () ->
    let command = _parse_command r in
    _assert_type command Getkeyrange_response;
    Session.incr_sequence client.session;
    let status = _get_status command in
    let code = _get_status_code status in
    match code with
    | Success ->
       let key_list = get_key_range_result command in
       Lwt_result.return key_list
    | x -> let sm = _get_status_message status in
           let code = status_code2i x in
           let e = Error.KineticError(code, sm) in
           Lwt_result.fail e

  let start_batch_operation
        ?timeout ?priority client =
    _assert_open client >>= fun () ->
    _assert_no_batch client >>= fun () ->
    let open Session in
    let session = client.session in
    let socket = client.socket in
    let batch_id = session.batch_id in
    let () = session.batch_id <- Int32.succ batch_id in
    let body = make_body () in
    let msg = make_start_batch ?timeout ?priority session batch_id ~body in
    network_send_generic I.write I.write_bytes socket msg None I.show_socket session.Session.trace >>= fun () ->
    let batch = B.make session socket batch_id in
    Session.incr_sequence session;
    Session.batch_on session;
    Lwt.return batch

  let end_batch_operation (batch:B.t)
    =
    Lwt_log.debug_f ~section "end_batch_operation" >>= fun () ->
    let open B in
    let msg = make_end_batch batch in
    let socket = batch.socket in
    let session = batch.session in
    let trace = session.Session.trace in
    network_send_generic I.write I.write_bytes socket msg None I.show_socket trace >>= fun () ->
    Session.incr_sequence session;
    Session.batch_off session;
    Lwt.return ()

  let make_batch_msg session
                     batch_id
                     message_type
                     entry
                     ~forced
      =
      
      let open Session in
      
      let header = make_header ~session ~message_type ~batch_id ~ack_sequence:session.sequence () in
      
      
      let open Entry in
      let maybe_tag = map_option snd entry.vt in
      let synchronization = Some WRITETHROUGH in
      let body = body_with_attributes ~ko:(Some entry.key)
                     ~db_version:entry.db_version
                     ~new_version:entry.new_version
                     ~forced
                     ~synchronization
                     ~maybe_tag ()
      in
      let command = make_command header body in
      
      let command_bytes = make_command_bytes command in
      let hmac = calculate_hmac session.secret command_bytes in
      let hmac_auth = {
          identity = Some session.identity;
          hmac = Some hmac;
        }
      in
      let m = {
          command_bytes = Some command_bytes;
          auth_type = Some Hmacauth;
          hmac_auth = Some hmac_auth;
          pin_auth = None;
        }
      in
      make_message_bytes m

  let batch_put
        (batch:B.t) entry
        ~forced
    =
    Lwt_log.debug_f ~section "batch_put %s" (Entry.show entry) >>= fun () ->
    let open Entry in
    begin
      match entry.vt with
      | None -> Lwt.return_unit (* why would it be None ? *)
      | Some ((v, v_off, v_len), t) -> _assert_value_size batch.B.session v_len
    end
    >>= fun () ->
    let open B in
    let msg = make_batch_msg
                batch.session
                batch.batch_id
                Put
                entry
                ~forced
    in
    let socket = batch.socket in
    let vo = map_option fst entry.vt in
    let session = batch.session in
    let trace = session.Session.trace in
    network_send_generic I.write I.write_bytes socket msg vo I.show_socket trace >>= fun () ->
    let () = B.inc_count batch in
    Session.incr_sequence session;
    Lwt_result.return ()


  let batch_delete
        (batch:B.t)
        entry
        ~forced
    =
    let open B in
    let msg = make_batch_msg batch.session
                             batch.batch_id
                             Delete
                             entry
                             ~forced
    in
    let socket = batch.socket in
    let session = batch.session in
    let trace = session.Session.trace in
    network_send_generic I.write I.write_bytes socket msg None I.show_socket trace >>= fun () ->
    let () = B.inc_count batch in
    Session.incr_sequence session;
    Lwt_result.return ()

  type forced = bool option
  type batch_operation =
    |BPut of Entry.t * forced
    |BDel of Entry.t * forced
           
  let do_batch ?(timeout:int64 option) ?priority client operations =
    _assert_open client >>= fun () ->
    start_batch_operation ?timeout ?priority client >>= fun batch ->

    let do_operation = function
      | BPut (pe, forced) -> batch_put    batch pe ~forced
      | BDel (de, forced) -> batch_delete batch de ~forced 
    in
    let rec loop cnt = function
      | [] -> Lwt_result.return cnt
      | op:: ops ->
         do_operation op >>=? fun () ->
         loop (cnt + 1) ops
    in
    loop 0 operations >>=? fun cnt ->
    end_batch_operation batch >>= fun () ->

    let session = client.session in
    let rec epilogue cnt =
      let read_msg socket =
        let timeout = session.config.timeout in
        network_receive_generic ~timeout I.create I.read I.read_bytes socket I.show_socket session.Session.trace
        >>=? fun (m, vo, proto_raw) ->
        Lwt_log.debug ~section "got msg" >>= fun () ->
        let auth_type = _get_message_auth_type (m:message) in
        let command = _parse_command m in
        begin
          match auth_type with
          | Pinauth | Invalid_auth_type -> Error.Generic(__FILE__,__LINE__, "bad auth type: " ^ to_hex (proto_raw)) |> Lwt_result.fail 
          | Unsolicitedstatus ->
             begin
               let () = Lwt_log.ign_info_f "unsolicitedstatus: %s" (to_hex proto_raw) in
               let status = _get_status command in
               let ccode = _get_status_code status in
               let sm = _get_status_message status in
               let rci = status_code2i ccode in
               Lwt_result.fail (Error.KineticError(rci,sm))
             end
          | Hmacauth ->
             begin
               begin
               try _get_message_type command |> Lwt_result.return
                 with
                 | exn ->
                    Error.Generic(__FILE__, __LINE__, "no type for: " ^ (to_hex proto_raw)) |> Lwt_result.fail
               end
               >>=? fun typ ->
               let typs = message_type2s typ in
               Lwt_log.debug_f "typs:%s" typs >>= fun () ->
               let status = _get_status command in
               let ccode = _get_status_code status in
               match ccode with
               | Success -> Lwt_result.return ()
               | _ ->
                  let sm = _get_status_message status in
                  let dsm = _get_detailed_status_message status in
                  Lwt_log.ign_debug_f ~section
                    "dsm:%S" (dsm |> Bytes.to_string);
                  let (rci:int) = status_code2i ccode in
                  Error.KineticError(rci,sm) |> Lwt_result.fail
             end
        end
      in 
      if cnt = 0
      then Lwt_result.return ()
      else
        Lwt_log.debug_f "count = %i" cnt >>= fun () ->
        read_msg batch.socket >>=? fun() ->
        epilogue (cnt -1)
    in
    epilogue 2

  let make_noop ?timeout ?priority session =
    let body = body_with_attributes ~ko:None
                            ~db_version:None
                            ~new_version:None
                            ~forced:None
                            ~synchronization:None
                            ~maybe_tag:None ()
    in
    make_serialized_msg ?timeout ?priority session Noop ~body

  (*

    let make_p2p_push session (host,port,tls) operations =
      let open Message_p2_poperation in
      let open Message_p2_poperation_operation in
      let open Message_p2_poperation_peer in
      let manip body =
        let poperations =
          List.map
          (fun ((k:string),(nko:string option)) ->
           let pop = default_message_p2_poperation_operation () in
           pop.key <- Some k;
           pop.new_key <- nko;
           pop
          ) operations
        in
        let ppeer = default_message_p2_poperation_peer () in
        let () = ppeer.hostname <- Some host in
        let () = ppeer.port <- Some (Int32.of_int port) in
        let () = ppeer.tls <- Some tls in

        let p2pop = default_message_p2_poperation () in
        let () = p2pop.peer <- Some ppeer in
        let () = p2pop.operation <- poperations in
        let () = body.p2p_operation <- Some p2pop in
        ()
      in
      make_serialized_msg session `peer2_peerpush manip

 *)
  let noop ?timeout ?priority client =
    _assert_open client >>= fun () ->
    _assert_no_batch client >>= fun () ->
    let msg = make_noop ?timeout ?priority client.session in
    let vo = None in
    _call client msg vo >>=? fun (r,vo, _) ->
    assert (vo = None);
    let command = _parse_command r in
    let () = Session.incr_sequence client.session in
    _assert_both r command Noop_response Success

  let make_instant_secure_erase session ~pin =


    let pin_op = {
        pin_op_type = Some Secure_erase_pinop;
      }
    in
    let body =
      body_with_attributes
        ~ko:None
        ~db_version:None
        ~new_version:None
        ~forced:None
        ~synchronization:None
        ~maybe_tag:None
        ~pin_op ()
    in
    
    make_pin_auth_serialized_msg
      session
      pin Pinop ~body

  let instant_secure_erase ?pin client =
    _assert_open client >>= fun () ->
    _assert_no_batch client >>= fun () ->
    let pin = get_option Bytes.empty pin in
    let msg = make_instant_secure_erase client.session ~pin in
    let vo = None in
    _call client msg vo >>=? fun(r,vo, _) ->
    assert (vo = None);
    let command = _parse_command r in
    let () = Session.incr_sequence client.session in
    _assert_both r command Pinop_response Success

  let make_download_firmware session =
    (*
        14984113
        authType: HMACAUTH
        hmacAuth {
          identity: 1
          hmac: "\323\240\'\215\267\302\246>*\335A\33461\372x;o\177\364"
        }
        commandBytes: "\n\020\010\000\030\230\361\254\314\262\361\247\341\n \0008\026\022\004\032\002(\001"

        header {
          clusterVersion: 0
          connectionID: 775357505907407000
          sequence: 0
          messageType: SETUP
        }
        body {
          setup {
            firmwareDownload: true
          }
        }

     *)

    
    let setup = {
        setup_op_type = Some Firmware_setupop;
        new_cluster_version = None;
      }
    in
    let body = make_body ~setup () in
    make_serialized_msg
      session Setup ~body


  let download_firmware client slod_data_slice =
    _assert_open client >>= fun () ->
    _assert_no_batch client >>= fun () ->
    let msg = make_download_firmware client.session in
    let vo = Some slod_data_slice in
    _call client msg vo >>=? fun (r, vo, proto_raw) ->
    assert (vo = None);
    let command = _parse_command r in
    let () = Session.incr_sequence client.session in
    _assert_both r command Setup_response Success
    (*

    let p2p_push session (ic,oc) peer operations =
      let msg = make_p2p_push session peer operations in
      network_send oc msg None >>= fun () ->
      network_receive ic >>= fun (r,vo) ->
      _assert_type r `peer2_peerpush_response;
      let () = incr_session session in
      let status = get_status_code r in
      let lwt_fail x = Lwt.fail(Failure x) in
      match status with
      | Success -> Lwt.return_unit
      | `invalid_status_code
      | `not_attempted
      | `hmac_failure
      | `not_authorized
      | `version_failure
      | `internal_error
      | `header_required
      | `not_found -> lwt_fail "not_found"
      | `version_mismatch -> lwt_fail "version_mismatch"
      | `service_busy -> lwt_fail "service_busy"
      | `expired -> lwt_fail "expired"
      | `data_error -> lwt_fail "data_error"
      | `perm_data_error -> lwt_fail "perm_data_error"
      | `remote_connection_error ->
         lwt_fail ("remote_connection_error: ")
      | `no_space -> lwt_fail "no_space"
      | `no_such_hmac_algorithm -> lwt_fail "no_such_hmac_algorithm"
      | `invalid_request ->
         let (int_status:int) = Obj.magic status in
         Lwt_io.printlf "x=%i\n%!" int_status >>= fun () ->
         let sm = get_status_message r in
         Lwt.fail (Failure sm)

 *)

  let wrap_socket ?trace ?timeout ?secret ?cluster_version ?max_operation_count_per_batch socket closer =
    let secret =
      match secret with
      | None -> "asdfasdf"
      | Some secret -> secret
    in
    let cluster_version =
      match cluster_version with
      | None -> 0L
      | Some cluster_version -> cluster_version
    in
    handshake secret cluster_version ?trace ?max_operation_count_per_batch socket ?timeout
    >>=? fun session ->
    Lwt_result.return {session; socket; closer; closed = false}

  let dispose (t:client) =
    Lwt_log.debug_f "dispose: %s" (bl2s (t.session.config.ipv4_addresses)) >>= fun () ->
    if not t.closed
    then
      begin
        t.closed <- true;
        t.closer ()
      end
    else Lwt.return_unit

  end
