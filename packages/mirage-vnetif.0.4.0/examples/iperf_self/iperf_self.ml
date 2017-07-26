(*
 * Copyright (c) 2011 Richard Mortier <mort@cantab.net>
 * Copyright (c) 2012 Balraj Singh <balraj.singh@cl.cam.ac.uk>
 * Copyright (c) 2015 Magnus Skjegstad <magnus@skjegstad.com>
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
 *)

open Lwt 
open Printf
open Gc
open String

module Main (C : Mirage_types_lwt.CONSOLE) = struct

  module S = struct
    module B = Basic_backend.Make
    module V = Vnetif.Make(B)
    module E = Ethif.Make(V)
    module I = Ipv4.Make(E)(Clock)(OS.Time)
    module U = Udp.Make(I)
    module T = Tcp.Flow.Make(I)(OS.Time)(Clock)(Random)
    module S = Tcpip_stack_direct.Make(C)(OS.Time)(Random)(V)(E)(I)(U)(T)
    include S
  end

  let or_error name fn t =
    fn t
    >>= function
        | `Error e -> fail (Failure ("Error starting " ^ name))
        | `Ok t -> return t 

  let create_stack c backend ip_config =
    or_error "backend" S.V.connect backend >>= fun netif ->
    C.log_s c (Printf.sprintf "Connected to backend with mac %s" (Macaddr.to_string (S.V.mac netif))) >>= fun () ->
    or_error "ethif" S.E.connect netif >>= fun ethif ->
    or_error "ipv4" S.I.connect ethif >>= fun ipv4 ->
    or_error "udpv4" S.U.connect ipv4 >>= fun udpv4 ->
    or_error "tcpv4" S.T.connect ipv4 >>= fun tcpv4 ->
    let config = {
        Mirage_types_lwt.name = "stack";
        Mirage_types_lwt.console = c; 
        Mirage_types_lwt.interface = netif;
        Mirage_types_lwt.mode = ip_config;
    } in
    or_error "stack" (S.connect config ethif ipv4 udpv4) tcpv4

  type stats = {
    mutable bytes: int64;
    mutable packets: int64;
    mutable bin_bytes:int64;
    mutable bin_packets: int64;
    mutable start_time: float;
    mutable last_time: float;
  }


  let msg = "01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890"

  let mlen = String.length msg

  let server_ready, server_ready_u = Lwt.wait ()
  let server_done, server_done_u = Lwt.wait ()

  let write_and_check flow buf =
    S.T.write flow buf >>= function
        | `Ok () -> Lwt.return_unit
        | `Eof -> S.T.close flow >> raise (Failure "EOF while writing to TCP flow")
        | `Error _ -> S.T.close flow >> raise (Failure "Error while writing to TCP flow")

  let tcp_connect t (ip, port) =
    S.T.create_connection t (ip, port) >>= function
        | `Error e -> raise (Failure (Printf.sprintf "Unable to connect to %s:%d" (Ipaddr.V4.to_string ip) port))
        | `Ok f -> Lwt.return f

  let iperfclient c s dest_ip dport =
    let iperftx flow =
      C.log_s c (Printf.sprintf "Iperf client: Made connection to server.%!") >>= fun () ->
      let a = Cstruct.sub (Io_page.(to_cstruct (get 1))) 0 mlen in
      Cstruct.blit_from_string msg 0 a 0 mlen;
      let amt = 1000000000 in
      for_lwt i = (amt / mlen) downto 1 do
        write_and_check flow a 
      done >>= fun () ->
      let a = Cstruct.sub a 0 (amt - (mlen * (amt/mlen))) in
      write_and_check flow a >>= fun () ->
      S.T.close flow
    in
    OS.Time.sleep 1. >>= fun () ->
    C.log_s c (Printf.sprintf "Iperf client: Attempting connection.%!") >>= fun () ->
    tcp_connect (S.tcpv4 s) (dest_ip, dport) >>= fun flow ->
    iperftx flow >>= fun () ->
    C.log_s c (Printf.sprintf "Iperf client: Done.%!")

  let print_data c st ts_now = 
    C.log_s c (Printf.sprintf "Iperf server: t = %f, rate = %Ld KBits/s, totbytes = %Ld, live_words = %d%!"
      (ts_now -. st.start_time)
      (Int64.of_float (((Int64.to_float st.bin_bytes) /. (ts_now -. st.last_time)) /. 125.))
      st.bytes Gc.((stat()).live_words)) >>= fun () ->
    st.last_time <- ts_now;
    st.bin_bytes <- 0L;
    st.bin_packets <- 0L;
    Lwt.return_unit


  let iperf c s flow =
    C.log_s c (Printf.sprintf "Iperf server: Received connection.%!") >>= fun () ->
    let t0 = Clock.time () in
    let st = {bytes=0L; packets=0L; bin_bytes=0L; bin_packets=0L; start_time = t0; last_time = t0} in
    let rec iperf_h flow =
      match_lwt (S.T.read flow) with
      | `Error _ -> raise (Failure "Unknown error in server while reading")
      | `Eof ->
          let ts_now = (Clock.time ()) in 
          st.bin_bytes <- st.bytes;
          st.bin_packets <- st.packets;
          st.last_time <- st.start_time;
          print_data c st ts_now >>= fun () ->
          S.T.close flow >>= fun () ->
          C.log_s c "Iperf server: Done - closed connection."
      | `Ok data -> begin
          let l = Cstruct.len data in
          st.bytes <- (Int64.add st.bytes (Int64.of_int l));
          st.packets <- (Int64.add st.packets 1L);
          st.bin_bytes <- (Int64.add st.bin_bytes (Int64.of_int l));
          st.bin_packets <- (Int64.add st.bin_packets 1L);
          let ts_now = (Clock.time ()) in 
          (if ((ts_now -. st.last_time) >= 1.0) then
                print_data c st ts_now
          else
                Lwt.return_unit) >>= fun () ->
          iperf_h flow
      end
    in
    iperf_h flow >>= fun () ->
    Lwt.wakeup server_done_u ();
    Lwt.return_unit

  let start c =
    let ip_server = Ipaddr.V4.of_string_exn "10.0.0.2" in
    let ip_config_server = `IPv4 (
      ip_server,
      Ipaddr.V4.of_string_exn "255.255.255.0",
      [Ipaddr.V4.of_string_exn "10.0.0.1"]
    ) in
    let ip_client = Ipaddr.V4.of_string_exn "10.0.0.1" in
    let ip_config_client = `IPv4 (
      Ipaddr.V4.of_string_exn "10.0.0.1",
      Ipaddr.V4.of_string_exn "255.255.255.0",
      [Ipaddr.V4.of_string_exn "10.0.0.1"]
    ) in
    let port = 5001 in
    let backend = (S.B.create ~yield:(fun () -> OS.Time.sleep 0.0) ~use_async_readers:true ()) in
    create_stack c backend ip_config_server >>= fun server_s ->
    create_stack c backend ip_config_client >>= fun client_s ->

    Lwt.choose [
        (server_ready >>= fun () ->
         OS.Time.sleep 1.0 >>= fun() ->
         C.log_s c (Printf.sprintf "I am client with IP %s, trying to connect to server @ %s:%d" (Ipaddr.V4.to_string ip_client) (Ipaddr.V4.to_string ip_server) port) >>= fun () ->
         iperfclient c client_s ip_server port) ;
        (OS.Time.sleep 1.0 >>= fun () ->
         C.log_s c (Printf.sprintf "I am server with IP %s, expecting connections on port %d" (Ipaddr.V4.to_string ip_server) port) >>= fun () ->
         S.listen_tcpv4 server_s ~port (iperf c server_s);
         Lwt.wakeup server_ready_u ();
         S.listen server_s) ] >>= fun () ->
    C.log_s c "Waiting for server_done..." >>= fun () ->
    server_done >>= fun () ->
    Lwt.return_unit (* exit cleanly *)

end
