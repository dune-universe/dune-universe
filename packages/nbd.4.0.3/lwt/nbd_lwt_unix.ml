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

(** Mostly concrete implementations for creating various kinds of channels.
  * Also exposes Nbd.Client and Nbd.Server as Client and Server submodules. *)

open Nbd
open Channel

let return = Lwt.return
let (>>=) = Lwt.(>>=)

type tls_role =
  | TlsClient of Ssl.context
  | TlsServer of Ssl.context

(* XXX Consider moving this function into a library also used by vhd-tool. *)
(* Also the read/write/close functions that are inside tls_channel_of_fd. *)
(** Call the io function [op] repeatedly until it reports that in
    total it has handled enough data to reach the end of the [buffer]
    (or call [Lwt.fail End_of_file] if [op] ceases to make progress).
  * The function [op] is an operation (such as a read/write/skip)
    that takes [fd], a buffer, an offset and a length, and returns
    an [int Lwt.t] to say how much data it processed.
  * [fd] is an open file descriptor.
  * [buffer] contains [buffer.Cstruct.buffer] for the data to be processed,
    from offset [buffer.Cstruct.off] to the end ([buffer.Cstruct.len]).
*)
let io_complete op fd buffer =
  let ofs = buffer.Cstruct.off in
  let len = buffer.Cstruct.len in
  let buf = buffer.Cstruct.buffer in
  (* loop returns the total of the ints returned by the calls to [op] *)
  let rec loop acc ofs len =
    op fd buf ofs len >>= fun n ->
    let len' = len - n in
    let acc' = acc + n in
    if len' = 0 || n = 0
    then return acc'
    else loop acc' (ofs + n) len' in
  loop 0 ofs len >>= fun n ->
  if n = 0 && len <> 0
  then Lwt.fail End_of_file
  else return ()

let tls_channel_of_fd fd role () =
  let ctx, ssl_start =
    match role with
      | TlsClient ctx -> ctx, Lwt_ssl.ssl_connect
      | TlsServer ctx -> ctx, Lwt_ssl.ssl_accept
  in
  ssl_start fd ctx >>= fun sock ->

  let read_tls buf =
    io_complete Lwt_ssl.read_bytes sock buf >>= fun () ->
    return () in

  let write_tls buf =
    io_complete Lwt_ssl.write_bytes sock buf >>= fun () ->
    return () in

  let close_tls () =
    ignore (Lwt_ssl.ssl_shutdown sock);
    Lwt_ssl.close sock in

  return { read_tls; write_tls; close_tls }


let cleartext_channel_of_fd fd role_opt =
  let read_clear = Lwt_cstruct.(complete (read fd)) in
  let write_clear = Lwt_cstruct.(complete (write fd)) in
  let close_clear () = Lwt_unix.close fd in
  let make_tls_channel = match role_opt with
    | None -> None
    | Some role -> Some (tls_channel_of_fd fd role)
  in
  { read_clear; write_clear; close_clear; make_tls_channel }

let generic_channel_of_fd fd role =
  let ch = cleartext_channel_of_fd fd role
  in return (Channel.generic_of_cleartext_channel ch)

(* This function is used by the client. The channel has no TLS ability. *)
let connect hostname port =
  let socket = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.gethostbyname hostname
  >>= fun host_info ->
  let server_address = host_info.Lwt_unix.h_addr_list.(0) in
  Lwt.catch (fun () ->
      Lwt_unix.connect socket (Lwt_unix.ADDR_INET (server_address, port))
    ) (fun e ->
      Lwt_unix.close socket >>= fun () -> Lwt.fail e
    )
  >>= fun () ->
  (generic_channel_of_fd socket None)

let init_tls_get_ctx ?curve ~certfile ~ciphersuites =
  Ssl_threads.init ();
  Ssl.init ();
  let mk_ctx role_ctx = Ssl.create_context Ssl.TLSv1_2 role_ctx in
  let ctx = mk_ctx Ssl.Server_context in
  Ssl.use_certificate ctx certfile certfile; (* Second one is being used as privkey filename *)
  Ssl.set_cipher_list ctx ciphersuites;
  begin match curve with
  | None -> ()
  | Some curve -> Ssl.init_ec_from_named_curve ctx curve;
  end;
  ctx

let with_block filename f =
  Block.connect filename
  >>= fun b ->
  Lwt.finalize
    (fun () -> f b)
    (fun () -> Block.disconnect b)

let ignore_exn t () = Lwt.catch t (fun _ -> Lwt.return_unit)

let with_channel fd tls_role f =
  let clearchan = cleartext_channel_of_fd fd tls_role in
  Lwt.finalize
    (fun () -> f clearchan)
    (* We use ignore_exn lest clearchan was closed already by f. *)
    (ignore_exn (fun () -> clearchan.close_clear ()))

module Client = Nbd.Client
module Server = Nbd.Server
