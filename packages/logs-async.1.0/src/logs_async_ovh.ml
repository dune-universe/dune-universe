(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Core
open Async

let reporter ~uri ~token =
  let module Logaas = Record.Make(Capnp.BytesMessage) in
  let open Logaas.Builder in
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    Fmt.with_buffer ~like b,
    fun () -> let m = Buffer.contents b in Buffer.reset b; m
  in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
  let reporter = Logs_fmt.reporter ~app ~dst () in
  let r = Record.init_root () in
  let hostname = Unix.gethostname () in
  let pid = Unix.getpid () in
  let auth = Pair.init_root () in
  let v = Pair.value_init auth in
  Pair.key_set auth "X-OVH-TOKEN" ;
  Pair.Value.string_set v token ;
  Record.hostname_set r hostname ;
  Record.procid_set r (Pid.to_string pid) ;
  Record.facility_set_exn r 0x3 ; (* daemon *)
  let _ = Record.pairs_set_list r [ auth ] in
  (* let service = Option.(Uri.port uri >>| Int.to_string) in
   * let sock = Socket.(create Type.udp) in
   * begin
   *   Unix.Addr_info.get ?service
   *     ~host:(Uri.host_with_default ~default:"" uri)
   *     [AI_SOCKTYPE SOCK_DGRAM] >>= function
   *   | { ai_addr = ADDR_INET (h, port) ; _ } :: _ ->
   *     Socket.connect sock (Socket.Address.Inet.create h port)
   *   | _ -> failwith "ovh_reporter: name resolve failed"
   * end >>= fun sock ->
   * let fd = Socket.fd sock in *)
  let host = Uri.host_with_default ~default:"" uri in
  let port = Option.value ~default:0 (Uri.port uri) in
  let hp = Host_and_port.create ~host ~port in
  let tcp_hp = Tcp.Where_to_connect.of_host_and_port hp in
  (* let rp = Reader.pipe tcp_r in
   * don't_wait_for @@ Pipe.iter rp ~f:begin fun msg ->
   *   Format.eprintf "%s" msg ;
   *   Deferred.unit
   * end ; *)
  (* let send_fun =
   *   match Udp.send () with
   *   | Ok a -> a
   *   | Error err -> raise (Error.to_exn err) in *)
  let stdout = Lazy.force Writer.stdout in
  let stderr = Lazy.force Writer.stderr in
  let send_fun =
    let conn = Mvar.create () in
    let rec loop_connect () =
      Monitor.try_with begin fun () ->
        Tcp.connect tcp_hp >>= fun (_, r, w) ->
        let app_to_ssl, app_to_ssl_w = Pipe.create () in
        let ssl_to_app_r, ssl_to_app = Pipe.create () in
        let net_to_ssl = Reader.pipe r in
        let ssl_to_net = Writer.pipe w in
        begin Async_ssl.Std.Ssl.client
            ~app_to_ssl ~ssl_to_app
            ~net_to_ssl ~ssl_to_net () >>= function
          | Error err ->
            Format.eprintf "%a" Error.pp err ;
            Error.raise err
          | Ok conn -> return conn
        end >>= fun _ssl_conn ->
        Mvar.update conn ~f:(fun _ -> app_to_ssl_w) ;
        Deferred.any [ Reader.close_finished r ;
                       Writer.close_started w ] >>= fun () ->
        let _ = Mvar.take_now_exn conn in
        Pipe.close_read app_to_ssl ;
        Pipe.close_read ssl_to_app_r ;
        Pipe.close_read net_to_ssl ;
        Pipe.close ssl_to_net ;
        Pipe.close app_to_ssl_w ;
        Pipe.close ssl_to_app ;
        begin if not (Reader.is_closed r) then
            Reader.close r else Deferred.unit
        end >>= fun () ->
        begin if not (Writer.is_closed w) then
            Writer.close w else Deferred.unit
        end
      end >>= fun _ ->
      Clock_ns.after (Time_ns.Span.of_int_sec 3) >>=
      loop_connect in
    don't_wait_for @@ loop_connect () ;
    fun level msg_full r_msg ->
      Mvar.value_available conn >>= fun () ->
      let w = Mvar.peek_exn conn in
      begin match level with
        | Logs.App -> Writer.write stdout msg_full
        | _ -> Writer.write stderr msg_full
      end ;
      Pipe.write w r_msg
  in
  let syslog_of_level = function
    | Logs.App -> 5
    | Error -> 3
    | Warning -> 4
    | Info -> 6
    | Debug -> 7 in
  let one_sec_in_ns_int63 = Int63.of_int_exn 1_000_000_000 in
  let report src level ~over k msgf =
    let k () =
      let ts = Time_ns.now () in
      Record.appname_set r (Logs.Src.name src) ;
      Record.ts_set r
        Int63.(Time_ns.to_int63_ns_since_epoch ts // one_sec_in_ns_int63) ;
      Record.severity_set_exn r (syslog_of_level level) ;
      let msg_full =
        match level with App -> app_flush () | _ -> dst_flush () in
      let msg_short =
        if String.length msg_full < 80 then
          msg_full
        else
          String.subo ~len:80 msg_full in
      Record.msg_set r msg_short ;
      Record.full_msg_set r msg_full ;
      Record.msgid_set r Uuid.(create () |> to_string) ;
      let r_msg = Record.to_message r in
      let r_msg = Capnp.Codecs.serialize ~compression:`None r_msg in
      don't_wait_for @@ Monitor.protect begin fun () ->
        send_fun level msg_full r_msg
      end ~finally:begin fun () ->
        Writer.flushed stdout >>= fun () ->
        Writer.flushed stderr >>| fun () ->
        over ()
      end ;
      k () in
    reporter.Logs.report src level ~over:(fun () -> ()) k msgf;
  in
  Deferred.return { Logs.report = report }

(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
