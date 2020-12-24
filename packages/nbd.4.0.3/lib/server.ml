(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Lwt.Infix
open Protocol
open Channel
open Result

exception Client_requested_abort

type name = string

type t = {
  channel: channel;
  request: Cstruct.t; (* buffer used to read the request headers *)
  reply: Cstruct.t;   (* buffer used to write the response headers *)
  m: Lwt_mutex.t; (* prevents partial message interleaving *)
}

type size = int64

let close t = t.channel.close ()

let make channel =
  let request = Cstruct.create Request.sizeof in
  let reply = Cstruct.create Reply.sizeof in
  let m = Lwt_mutex.create () in
  { channel; request; reply; m }

let connect channel ?offer () =
  let section = Lwt_log_core.Section.make("Server.connect") in
  Lwt_log_core.notice ~section "Starting fixed-newstyle negotiation" >>= fun () ->

  (match offer with
  | Some offers ->
    Lwt_list.iter_s
      (fun name ->
         if String.length name > 4096 then Lwt.fail_with "export name must be no longer than 4096 bytes" else Lwt.return_unit)
      offers
  | None -> Lwt.return_unit) >>= fun () ->

  let buf = Cstruct.create Announcement.sizeof in
  Announcement.(marshal buf `V2);
  channel.write_clear buf
  >>= fun () ->
  let buf = Cstruct.create (Negotiate.sizeof `V2) in
  Negotiate.(marshal buf (V2 [ GlobalFlag.Fixed_newstyle ]));
  channel.write_clear buf
  >>= fun () ->
  let buf = Cstruct.create NegotiateResponse.sizeof in
  channel.read_clear buf
  >>= fun () ->
  (* Option negotiation *)
  let req = Cstruct.create OptionRequestHeader.sizeof in
  let res = Cstruct.create OptionResponseHeader.sizeof in
  let make_blank_payload hdr =
    Cstruct.create (Int32.to_int hdr.OptionRequestHeader.length)
  in
  let respond ?(len=0) opt resp writefn =
    OptionResponseHeader.(marshal res { request_type = opt; response_type = resp; length = Int32.of_int len });
    writefn res
  in
  let send_ack opt writefn = respond opt OptionResponse.Ack writefn
  in
  let read_hdr_and_payload readfn =
    readfn req >>= fun () ->
    match OptionRequestHeader.unmarshal req with
    | Error e -> Lwt.fail e
    | Ok hdr ->
      let payload = make_blank_payload hdr in
      readfn payload
      >>= fun () -> Lwt.return (hdr.OptionRequestHeader.ty, payload)
  in
  let generic_loop chan =
    let rec loop () =
      read_hdr_and_payload chan.read
      >>= fun (opt, payload) -> match opt with
      | Option.StartTLS ->
        let resp = if chan.is_tls then OptionResponse.Invalid else OptionResponse.Policy in
        respond opt resp chan.write
        >>= loop
      | Option.ExportName -> Lwt.return (Cstruct.to_string payload, make chan)
      | Option.Abort ->
        Lwt.catch
          (fun () -> send_ack opt chan.write)
          (fun exn -> Lwt_log_core.warning ~section ~exn "Failed to send ack after receiving abort")
        >>= fun () ->
        Lwt.fail Client_requested_abort
      | Option.Unknown _ ->
        respond opt OptionResponse.Unsupported chan.write
        >>= loop
      | Option.List ->
        begin match offer with
          | None ->
            respond opt OptionResponse.Policy chan.write
            >>= loop
          | Some offers ->
            let rec advertise = function
              | [] -> send_ack opt chan.write
              | x :: xs ->
                let len = String.length x in
                respond ~len:(len + 4) opt OptionResponse.Server chan.write
                >>= fun () ->
                let name = Cstruct.create (len + 4) in
                Cstruct.BE.set_uint32 name 0 (Int32.of_int len);
                Cstruct.blit_from_string x 0 name 4 len;
                chan.write name
                >>= fun () ->
                advertise xs
            in
            advertise offers
            >>= loop
        end
    in loop ()
  in
  let negotiate_tls make_tls_channel =
    let rec negotiate_tls () =
      read_hdr_and_payload channel.read_clear
      >>= fun (opt, _) -> match opt with
      | Option.ExportName -> Lwt.fail_with "Client requested export over cleartext channel but server is in FORCEDTLS mode."
      | Option.Abort -> Lwt.fail_with "Client requested abort (before negotiating TLS)."
      | Option.StartTLS -> (
          send_ack opt channel.write_clear
          >>= make_tls_channel
          >>= fun tch ->
          generic_loop (Channel.generic_of_tls_channel tch)
        )
      (* For any other option, respond saying TLS is required, then await next OptionRequest. *)
      | _ -> respond opt OptionResponse.TlsReqd channel.write_clear
        >>= negotiate_tls
    in negotiate_tls ()
  in
  let client_flags = NegotiateResponse.unmarshal buf in
  (* Does the client support Fixed_newstyle? *)
  let old_client = not (List.mem ClientFlag.Fixed_newstyle client_flags) in
  match channel.make_tls_channel with
  | None -> (    (* We are in NOTLS mode *)
      (if old_client
       then Lwt_log_core.warning ~section "Client doesn't report Fixed_newstyle"
       else Lwt.return_unit) >>= fun () ->
      (* Continue regardless *)
      generic_loop (Channel.generic_of_cleartext_channel channel)
    )
  | Some make_tls_channel -> (   (* We are in FORCEDTLS mode *)
      if old_client
      then (
        Lwt_log_core.error ~section "Server rejecting connection: it wants to use TLS but client flags don't include Fixed_newstyle" >>= fun () ->
        Lwt.fail_with "client does not report Fixed_newstyle and server is in FORCEDTLS mode."
      )
      else negotiate_tls make_tls_channel
    )

let with_connection clearchan ?offer f =
  connect clearchan ?offer ()
  >>= fun (exportname, t) ->
  Lwt.finalize
    (fun () -> f exportname t)
    (fun () -> close t)

let negotiate_end t  size flags : t Lwt.t =
  let buf = Cstruct.create DiskInfo.sizeof in
  DiskInfo.(marshal buf { size; flags });
  t.channel.write buf
  >>= fun () ->
  Lwt.return { channel = t.channel; request = t.request; reply = t.reply; m = t.m }

let next t =
  t.channel.read t.request
  >>= fun () ->
  match Request.unmarshal t.request with
  | Ok r -> Lwt.return r
  | Error e -> Lwt.fail e

let ok t handle payload =
  Lwt_mutex.with_lock t.m
    (fun () ->
       Reply.marshal t.reply { Reply.handle; error = Ok () };
       t.channel.write t.reply
       >>= fun () ->
       match payload with
       | None -> Lwt.return ()
       | Some data -> t.channel.write data
    )

let error t handle code =
  Lwt_mutex.with_lock t.m
    (fun () ->
       Reply.marshal t.reply { Reply.handle; error = Error code };
       t.channel.write t.reply
    )

let serve t (type t) ?(read_only=true) block (b:t) =
  let section = Lwt_log_core.Section.make("Server.serve") in
  let module Block = (val block: Mirage_block_lwt.S with type t = t) in

  Lwt_log_core.notice_f ~section "Serving new client, read_only = %b" read_only >>= fun () ->

  Block.get_info b
  >>= fun info ->
  let size = Int64.(mul info.Mirage_block.size_sectors (of_int info.Mirage_block.sector_size)) in
  (match read_only, info.Mirage_block.read_write with
   | true, _ -> Lwt.return true
   | false, true -> Lwt.return false
   | false, false ->
     Lwt_log_core.error ~section "Read-write access was requested, but block is read-only, sending NBD_FLAG_READ_ONLY transmission flag" >>= fun () ->
     Lwt.return true)
  >>= fun read_only ->
  let flags = if read_only then [ PerExportFlag.Read_only ] else [] in
  negotiate_end t size flags
  >>= fun t ->

  let block = Io_page.(to_cstruct (get 128)) in
  let block_size = Cstruct.len block in
  let rec loop () =
    next t
    >>= fun request ->
    let open Request in
    match request with
    | { ty = Command.Write; from; len; handle } ->
      if read_only
      then error t handle `EPERM
      else if Int64.(rem from (of_int info.Mirage_block.sector_size)) <> 0L || Int64.(rem (of_int32 len) (of_int info.Mirage_block.sector_size) <> 0L)
      then error t handle `EINVAL
      else begin
        let rec copy offset remaining =
          let n = min block_size remaining in
          let subblock = Cstruct.sub block 0 n in
          t.channel.Channel.read subblock
          >>= fun () ->
          Block.write b Int64.(div offset (of_int info.Mirage_block.sector_size)) [ subblock ]
          >>= function
          | Error e ->
            Lwt_log_core.debug_f ~section "Error while writing: %s; returning EIO error" (Fmt.to_to_string Block.pp_write_error e) >>= fun () ->
            error t handle `EIO
          | Ok () ->
            let remaining = remaining - n in
            if remaining > 0
            then copy Int64.(add offset (of_int n)) remaining
            else ok t handle None >>= fun () -> loop () in
        copy from (Int32.to_int request.Request.len)
      end
    | { ty = Command.Read; from; len; handle } ->
      (* It is okay to disconnect here in case of errors. The NBD protocol
         documentation says about NBD_CMD_READ:
         "If an error occurs, the server SHOULD set the appropriate error code
         in the error field. The server MAY then initiate a hard disconnect.
         If it chooses not to, it MUST NOT send any payload for this request.
         If an error occurs while reading after the server has already sent out
         the reply header with an error field set to zero (i.e., signalling no
         error), the server MUST immediately initiate a hard disconnect; it
         MUST NOT send any further data to the client." *)
      if Int64.(rem from (of_int info.Mirage_block.sector_size)) <> 0L || Int64.(rem (of_int32 len) (of_int info.Mirage_block.sector_size) <> 0L)
      then error t handle `EINVAL
      else begin
        ok t handle None
        >>= fun () ->
        let rec copy offset remaining =
          let n = min block_size remaining in
          let subblock = Cstruct.sub block 0 n in
          Block.read b Int64.(div offset (of_int info.Mirage_block.sector_size)) [ subblock ]
          >>= function
          | Error e ->
            Lwt.fail_with (Printf.sprintf "Partial failure during a Block.read: %s; terminating the session" (Fmt.to_to_string Block.pp_error e))
          | Ok () ->
            t.channel.write subblock
            >>= fun () ->
            let remaining = remaining - n in
            if remaining > 0
            then copy Int64.(add offset (of_int n)) remaining
            else loop () in
        copy from (Int32.to_int request.Request.len)
      end
    | { ty = Command.Disc; _ } ->
      Lwt_log.notice ~section "Received NBD_CMD_DISC, disconnecting" >>= fun () ->
      Lwt.return_unit
    | _ ->
      Lwt_log_core.warning ~section "Received unknown command, returning EINVAL" >>= fun () ->
      error t request.Request.handle `EINVAL in
  loop ()
