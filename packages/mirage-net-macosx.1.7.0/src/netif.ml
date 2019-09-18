(*
 * Copyright (c) 2010-2014 Anil Madhavapeddy <anil@recoil.org>
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

open Lwt.Infix
open Mirage_net

let src = Logs.Src.create "net-macosx" ~doc:"OSX network device"
module Log = (val Logs.src_log src : Logs.LOG)

type +'a io = 'a Lwt.t

type error = Mirage_net.Net.error
let pp_error = Mirage_net.Net.pp_error

type t = {
  id: string;
  mutable active: bool;
  mac: Macaddr.t;
  mtu: int;
  max_packet_size: int;
  stats : Mirage_net.stats;
  dev: Lwt_vmnet.t;
}

let connect _ =
  Lwt_vmnet.init () >|= fun dev ->
  let devname = "unknown" in (* TODO fix *)
  let mac = Lwt_vmnet.mac dev in
  let mtu = Lwt_vmnet.mtu dev in
  let max_packet_size = Lwt_vmnet.max_packet_size dev in
  let active = true in
  let t = {
    id = devname; dev; active; mac; mtu; max_packet_size;
    stats = { rx_bytes=0L;rx_pkts=0l;
              tx_bytes=0L; tx_pkts=0l }}
  in
  Log.info (fun l -> l "Netif: connect %s" devname);
  t

let disconnect t =
  Log.info (fun l -> l "Netif: disconnect %s" t.id);
  t.active <- false;
  Lwt.return_unit

type macaddr = Macaddr.t
type buffer = Cstruct.t

(* Input a frame, and block if nothing is available *)
let read t buf =
  Lwt.catch (fun () -> Lwt_vmnet.read t.dev buf >|= fun c -> `Ok c)
    (function
      | Lwt_vmnet.Error e ->
        Log.err (fun l -> l "read: %s"
          (Sexplib.Sexp.to_string_hum (Lwt_vmnet.sexp_of_error e)));
        Lwt.return (`Error `Disconnected)
      | e -> Lwt.fail e)

(* Loop and listen for packets permanently *)
let rec listen t ~header_size fn =
  match t.active with
  | false -> Lwt.return (Error `Disconnected)
  | true  ->
    Lwt.catch (fun () ->
        let buf = Cstruct.create t.max_packet_size in
        read t buf >|= function
        | `Error e ->
          Log.err (fun l -> l "Netif: error, terminating listen loop");
          Error e
        | `Ok buf ->
          Lwt.ignore_result (
            Lwt.catch (fun () -> fn buf)
              (fun exn ->
                 Log.err (fun l ->
                     l "EXN: %a bt: %s" Fmt.exn exn (Printexc.get_backtrace()));
                 Lwt.return_unit)
          );
          Ok ()
      ) (function
        | Lwt.Canceled ->
          Log.info (fun l ->
              l "[netif-input] listen function canceled, terminating");
          Lwt.return (Error `Disconnected)
        | exn ->
          Log.err (fun l -> l "[netif-input] error : %a" Fmt.exn exn);
          Lwt.return (Ok ()))
    >>= function
    | Ok ()        -> (listen[@tailcall]) t ~header_size fn
    | Error _ as e -> Lwt.return e

let write t ~size fillf =
  let buf = Cstruct.create size in
  let len = fillf buf in
  if len > size then
    Lwt.return (Error `Invalid_length)
  else
    let buf = Cstruct.sub buf 0 len in
    Lwt_vmnet.write t.dev buf >|= fun () ->
    t.stats.tx_pkts <- Int32.succ t.stats.tx_pkts;
    t.stats.tx_bytes <- Int64.add t.stats.tx_bytes (Int64.of_int len);
    Ok ()

let mac t = t.mac

let mtu t = t.mtu

let get_stats_counters t = t.stats

let reset_stats_counters t =
  t.stats.rx_bytes <- 0L;
  t.stats.rx_pkts  <- 0l;
  t.stats.tx_bytes <- 0L;
  t.stats.tx_pkts  <- 0l
