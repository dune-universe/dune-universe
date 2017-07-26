(*
 * Copyright (C) 2016 David Scott <dave@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Lwt.Infix

let read_lines filename =
  Lwt.catch
    (fun () ->
      Lwt_io.open_file filename ~mode:Lwt_io.input
      >>= fun ic ->
      Lwt.finalize
        (fun () ->
          let s = Lwt_io.read_lines ic in
          Lwt_stream.to_list s
        ) (fun () ->
          Lwt_io.close ic
        )
    ) (function
      | Unix.Unix_error(Unix.ENOENT, _, _) as e ->
        Logs.err (fun f -> f "failed to find file %s" filename);
        Lwt.fail e
      | e ->
        Lwt.fail e
      )

module Time = struct
  type 'a io = 'a Lwt.t
  let sleep = Lwt_unix.sleep
end

open Dns_forward
open Dns_forward_lwt_unix

let max_udp_length = 65507

let set_signal_if_supported signal handler =
  try
    Sys.set_signal signal handler
  with Invalid_argument _ ->
    ()

let serve port filename =
  set_signal_if_supported Sys.sigpipe Sys.Signal_ignore;

  if filename = "" then begin
    `Error (true, "please supply the name of a config file")
  end else Lwt_main.run begin
    read_lines filename
    >>= fun lines ->
    let all = String.concat "\n" lines in
    match Config.of_string all with
    | Result.Error (`Msg m) -> Lwt.return (`Error(true, m))
    | Result.Ok config ->
      let open Lwt.Infix in
      Resolver.Udp.create config
      >>= fun udp_resolver ->
      Server.Udp.create udp_resolver
      >>= fun udp ->
      Resolver.Tcp.create config
      >>= fun tcp_resolver ->
      Server.Tcp.create tcp_resolver
      >>= fun tcp ->
      let address = { Config.Address.ip = Ipaddr.V4 Ipaddr.V4.localhost; port } in
      let t =
        let open Error.Infix in
        Server.Udp.serve ~address udp
        >>= fun () ->
        Server.Tcp.serve ~address tcp
        >>= fun () ->
        let t, _ = Lwt.task () in
        t in
      t >>= function
      | Result.Error (`Msg m) -> Lwt.return (`Error(true, m))
      | Result.Ok () -> Lwt.return (`Ok ())
  end
