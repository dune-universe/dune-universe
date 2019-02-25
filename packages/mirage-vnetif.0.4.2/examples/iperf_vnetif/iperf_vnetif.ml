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

  module B = Basic_backend.Make
  module V = Vnetif.Make(B)

  let or_error name fn t =
    fn t
    >>= function
        | `Error e -> fail (Failure ("Error starting " ^ name))
        | `Ok t -> return t 

  let create_stack c backend =
    or_error "backend" V.connect backend >>= fun netif ->
    C.log_s c (Printf.sprintf "Connected to backend with mac %s" (Macaddr.to_string (V.mac netif))) >>= fun () ->
    Lwt.return netif

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

  let total_len = 5000000000

  let server_ready, server_ready_u = Lwt.wait ()
  let client_done, client_done_u = Lwt.wait ()

  let iperfclient c s =
    C.log_s c (Printf.sprintf "Iperf client: Sending data to server by calling vnetif.write.%!") >>= fun () ->
    let a = Cstruct.create mlen in
    Cstruct.blit_from_string msg 0 a 0 mlen;
    let amt = total_len in
    for_lwt i = (amt / mlen) downto 1 do
        V.write s a 
    done >>= fun () ->
    let a = Cstruct.sub a 0 (amt - (mlen * (amt/mlen))) in
    V.write s a >>= fun () ->
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

  let iperf c s =
    C.log_s c (Printf.sprintf "Iperf server: Ready to receive data.%!") >>= fun () ->
      Lwt.wakeup server_ready_u ();
    let t0 = Clock.time () in
    let st = {bytes=0L; packets=0L; bin_bytes=0L; bin_packets=0L; start_time = t0; last_time = t0} in
    let enough_data, enough_data_waker = Lwt.wait () in
    Lwt.choose [
       V.listen s (fun data -> 
            let l = Cstruct.len data in
            st.bytes <- (Int64.add st.bytes (Int64.of_int l));
            st.packets <- (Int64.add st.packets 1L);
            st.bin_bytes <- (Int64.add st.bin_bytes (Int64.of_int l));
            st.bin_packets <- (Int64.add st.bin_packets 1L);
            let ts_now = (Clock.time ()) in 
            (if ((ts_now -. st.last_time) >= 1.0) || (Int64.to_int st.bytes) >= total_len then
                print_data c st ts_now >>= fun () ->
                (if ((Int64.to_int st.bytes) >= total_len) then
                    Lwt.wakeup enough_data_waker ()); 
                Lwt.return_unit
            else
                Lwt.return_unit))
      ;
      enough_data ] (* wait for client *)

  let start c =
    let backend = (B.create ~use_async_readers:false ()) in
    create_stack c backend >>= fun server_s ->
    create_stack c backend >>= fun client_s ->

    Lwt.join [
        (server_ready >>= fun () ->
         C.log_s c (Printf.sprintf "Client starting...%!") >>= fun () ->
         iperfclient c client_s >>= fun () ->
         Lwt.wakeup client_done_u (); 
         Lwt.return_unit
         ) ;
        (C.log_s c (Printf.sprintf "Server starting...%!") >>= fun () ->
         iperf c server_s) ] >>= fun () ->
    Lwt.return_unit (* exit cleanly *)

end
