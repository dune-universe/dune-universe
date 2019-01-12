(*
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

let red fmt    = Printf.sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt  = Printf.sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = Printf.sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt   = Printf.sprintf ("\027[36m"^^fmt^^"\027[m")

let netmask = Ipaddr.V4.of_string_exn "255.255.255.0" 
let gw = Ipaddr.V4.of_string_exn "192.168.56.1" 

module Main (C: Mirage_types_lwt.CONSOLE) = struct

  module Stack = struct
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

  let accept c flow =
    let ip, port = Stack.TCPV4.get_dest flow in
    C.log_s c (green "Accepted connection from %s:%d%!" (Ipaddr.V4.to_string ip) port) >>= fun () ->
    Stack.TCPV4.read flow >>= (function
        | `Ok b -> C.log_s c (green "Got %s%!" (Cstruct.to_string b))
        | `Eof | `Error _ -> C.log_s c (green "Error while reading%!")) >>= fun () ->
    Stack.TCPV4.close flow >>= fun () ->
    C.log_s c (green "Connection closed%!")

  let create_stack c backend ip =
    or_error "backend" Stack.V.connect backend >>= fun netif ->
    C.log_s c (blue "Connected to backend with mac %s%!" (Macaddr.to_string (Stack.V.mac netif))) >>= fun () ->
    or_error "ethif" Stack.E.connect netif >>= fun ethif ->
    or_error "ipv4" Stack.I.connect ethif >>= fun ipv4 ->
    or_error "udpv4" Stack.U.connect ipv4 >>= fun udpv4 ->
    or_error "tcpv4" Stack.T.connect ipv4 >>= fun tcpv4 ->
    let config = {
        Mirage_types_lwt.name = "stack";
        Mirage_types_lwt.console = c; 
        Mirage_types_lwt.interface = netif;
        Mirage_types_lwt.mode = `IPv4 (Ipaddr.V4.of_string_exn ip, netmask, [gw]);
    } in
    or_error "stack" (Stack.connect config ethif ipv4 udpv4) tcpv4
  
  let start c =
    let backend = Stack.B.create ~use_async_readers:true ~yield:(fun() -> OS.Time.sleep 0.0) () in (* use_async_readers must be true with tcpip *)
    Lwt.pick [
        (create_stack c backend "192.168.56.99" >>= fun s1 ->
        Stack.listen_tcpv4 s1 ~port:80 (fun f -> accept c f);
        Stack.listen s1) ;

        (OS.Time.sleep 3.0 >>= fun () ->
        create_stack c backend "192.168.56.98" >>= fun s2 ->
        or_error "connect" (Stack.TCPV4.create_connection (Stack.tcpv4 s2)) ((Ipaddr.V4.of_string_exn "192.168.56.99"), 80) >>= fun flow ->
        C.log_s c (yellow "Connected to other end...%!") >>= fun () ->
        Stack.TCPV4.write flow (Cstruct.of_string "hello world 1 2 3 4 5") >>= (function
            | `Ok () -> C.log_s c (yellow  "wrote hello world%!")
            | `Error _ -> C.log_s c (yellow "tried to write, got error%!")
            | `Eof -> C.log_s c (yellow "tried to write, got eof%!")) >>= fun () ->
        Stack.TCPV4.close flow >>= fun () ->
        Lwt.return_unit) ] >>= fun () ->
    Lwt.return_unit

end
