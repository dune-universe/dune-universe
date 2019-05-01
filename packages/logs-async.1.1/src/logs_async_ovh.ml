(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Core
open Async

let uri_token_arg = Command.Arg_type.create begin fun s ->
    match String.split s ~on:',' with
    | uri :: token :: _ -> Uri.of_string uri, token
    | _ -> invalid_arg "uri_token"
  end

let ovh_logs =
  let open Command.Param in
  flag "ovh-logs" (optional uri_token_arg)
    ~doc:"url,token Credentials for OVH log service"

let ovhtoken =
  Logs.Tag.def ~doc:"OVH id token" "X-OVH-TOKEN" Format.pp_print_string

let send_tcp_tls uri =
  let stdout = Lazy.force Writer.stdout in
  let stderr = Lazy.force Writer.stderr in
  let conn = Mvar.create () in
  let host = Uri.host_with_default ~default:"" uri in
  let port = Option.value ~default:0 (Uri.port uri) in
  let hp = Host_and_port.create ~host ~port in
  let tcp_hp = Tcp.Where_to_connect.of_host_and_port hp in
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
  return @@ fun level msg ->
  let msg_str = Format.asprintf "%a@." Rfc5424.pp msg in
  Mvar.value_available conn >>= fun () ->
  let w = Mvar.peek_exn conn in
  begin match level with
    | Logs.App -> Writer.write stdout msg_str
    | _ -> Writer.write stderr msg_str
  end ;
  Pipe.write w msg_str

let send_udp uri =
  let stdout = Lazy.force Writer.stdout in
  let stderr = Lazy.force Writer.stderr in
  let service = Option.(Uri.port uri >>| Int.to_string) in
  let sock = Socket.(create Type.udp) in
  begin
    Unix.Addr_info.get ?service
      ~host:(Uri.host_with_default ~default:"" uri)
      [AI_SOCKTYPE SOCK_DGRAM] >>= function
    | { ai_addr = ADDR_INET (h, port) ; _ } :: _ ->
      Socket.connect sock (Socket.Address.Inet.create h ~port)
    | _ -> failwith "ovh_reporter: name resolve failed"
  end >>| fun sock ->
  let fd = Fd.file_descr_exn (Socket.fd sock) in
  let write_iobuf =
    match Iobuf.send_nonblocking_no_sigpipe () with
    | Error _e -> failwith "send_fun"
    | Ok f -> f in
  fun level msg ->
    let msg = Format.asprintf "%a@." Rfc5424.pp msg in
    begin match level with
      | Logs.App -> Writer.write stdout msg
      | _ -> Writer.write stderr msg
    end ;
    let iobuf = Iobuf.of_string msg in
    let _ = write_iobuf iobuf fd in
    Deferred.unit

let maybe_send f uri =
  let stdout = Lazy.force Writer.stdout in
  let stderr = Lazy.force Writer.stderr in
  match uri with
  | None ->
    Deferred.return begin
      fun level msg ->
        let msg = Format.asprintf "%a@." Rfc5424.pp msg in
        begin match level with
          | Logs.App -> Writer.write stdout msg
          | _ -> Writer.write stderr msg
        end ;
        Deferred.unit
    end
  | Some (uri, _) -> f uri

let warp10 (type a) (t:a Rfc5424.Tag.typ) (v:a) =
  match t with
  | Rfc5424.Tag.String -> Warp10.String v
  | Rfc5424.Tag.Bool   -> Warp10.Bool v
  | Rfc5424.Tag.Float  -> Warp10.Double v
  | Rfc5424.Tag.I64    -> Warp10.Long v
  | Rfc5424.Tag.U64    -> Warp10.Long (Uint64.to_int64 v)
  | Rfc5424.Tag.U      -> Warp10.Bool true

let warp10_of_tags defs tags =
  let open Rfc5424 in
  let q = Queue.create () in
  List.iter defs ~f:begin fun ((Tag.Dyn (t, _d)) as tydef) ->
    match Tag.find t tydef tags with
    | None -> ()
    | Some (_, None) -> ()
    | Some (d, Some v) ->
      Warp10.create ~name:(Logs.Tag.name d) (warp10 t v) |>
      Queue.enqueue q
  end ;
  q

let make_reporter ?(defs=[]) ?logs ?metrics make_f =
  let p =
    Option.map metrics ~f:begin fun (uri, token) ->
      let warp10_r, warp10_w = Pipe.create () in
      Warp10_async.record ~uri ~token warp10_r ;
      warp10_w
    end in
  let send_metrics_from_tags tags =
    match p with
    | Some p when (not (Pipe.is_closed p)) -> begin
        Monitor.try_with_or_error begin fun () ->
          Pipe.transfer_in p ~from:(warp10_of_tags defs tags)
        end >>= function
        | Error e ->
          Logs_async.err (fun m -> m "%a" Error.pp e)
        | Ok () -> Deferred.unit
      end
    | _ -> Deferred.unit in
  let tokens =
    match logs with
    | None -> Logs.Tag.empty
    | Some (_, token) -> Logs.Tag.(add ovhtoken token empty) in
  let tokens =
    if Logs.Tag.is_empty tokens then []
    else
      [Rfc5424.create_sd_element
         ~defs:[Rfc5424.Tag.string ovhtoken]
         ~section:"tokens" ~tags:tokens] in
  let hostname = Unix.gethostname () in
  let app_name = Filename.basename Sys.executable_name in
  let procid = Pid.to_string (Unix.getpid ()) in
  let pf = Rfc5424.create ~hostname ~procid in
  let stdout = Lazy.force Writer.stdout in
  let stderr = Lazy.force Writer.stderr in
  make_f >>= fun f ->
  let report src level ~over k msgf =
    let m ?header:_ ?(tags=Logs.Tag.empty) fmt =
      let othertags =
        Rfc5424.create_sd_element ~defs ~section:"logs" ~tags in
      let structured_data =
        if Logs.Tag.is_empty tags
        then tokens
        else othertags :: tokens in
      let pf = pf
        ~severity:(Rfc5424.severity_of_level level)
        ~app_name:(app_name ^ "." ^ Logs.Src.name src)
        ~structured_data ~ts:(Ptime_clock.now ()) in
      let k msg =
        don't_wait_for @@
        Monitor.protect begin fun () ->
          send_metrics_from_tags tags >>= fun () ->
          f level (pf ~msg ())
        end
          ~finally:begin fun () ->
          Writer.flushed stdout >>= fun () ->
          Writer.flushed stderr >>| fun () ->
          over ()
        end ;
        k () in
      Format.kasprintf k fmt
    in
    msgf m
  in
  Deferred.return { Logs.report = report }

let udp_reporter ?defs ?logs ?metrics () =
  make_reporter ?defs ?logs ?metrics (maybe_send send_udp logs)

let tcp_tls_reporter ?defs ?logs ?metrics () =
  make_reporter ?defs ?logs ?metrics (maybe_send send_tcp_tls logs)

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
