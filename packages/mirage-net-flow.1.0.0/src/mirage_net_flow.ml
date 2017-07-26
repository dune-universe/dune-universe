(*
 * Copyright (c) 2010-2015 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (C) 2015      Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Result
open Mirage_net

let src = Logs.Src.create "mirage-net-flow"
module Log = (val Logs.src_log src : Logs.LOG)

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

module Make (F: Mirage_flow_lwt.S) = struct

  type +'a io = 'a Lwt.t

  type t = {
    id: int;
    flow: F.flow;
    mutable active: bool;
    mutable mac: Macaddr.t;
    stats : Mirage_net.stats;
  }

  type error = [
    | Mirage_net.error
    | `Flow of F.write_error
  ]

  let pp_error ppf = function
    | #Mirage_net.error as e -> Mirage_net.pp_error ppf e
    | `Flow e -> F.pp_write_error ppf e

  let devices = Hashtbl.create 1
  let () = Random.self_init ()

  let connect ?mac flow =
    let id = Random.int 1024 in
    let mac = match mac with
      | None   -> Macaddr.make_local (fun _ -> Random.int 256)
      | Some m -> m
    in
    let active = true in
    let t = {
      flow; id; active; mac;
      stats= { rx_bytes=0L;rx_pkts=0l; tx_bytes=0L; tx_pkts=0l } }
    in
    Hashtbl.add devices id t;
    Log.debug
      (fun l -> l "connect netif.%d with mac %s" id (Macaddr.to_string mac));
    Lwt.return t

  let disconnect t =
    Log.debug (fun l -> l "disconnect netif.%d" t.id);
    t.active <- false;
    F.close t.flow

  type macaddr = Macaddr.t
  type page_aligned_buffer = Io_page.t
  type buffer = Cstruct.t

  (* Input a frame, and block if nothing is available *)
  let rec read t =
    let process () =
      F.read t.flow >|= function
      | Ok `Eof        -> Error `Disconnected
      | Ok (`Data buf) ->
        let len = Cstruct.len buf in
        t.stats.rx_pkts <- Int32.succ t.stats.rx_pkts;
        t.stats.rx_bytes <- Int64.add t.stats.rx_bytes (Int64.of_int len);
        Ok buf
      | Error e ->
        Log.debug (fun l -> l "[read] error: %a, continuing" F.pp_error e);
        Error `Continue
    in
    process () >>= function
    | Error `Continue     -> read t
    | Error `Disconnected -> Lwt.return (Error `Disconnected)
    | Ok buf              -> Lwt.return (Ok buf)

  let safe_apply f x =
    Lwt.catch
      (fun () -> f x)
      (fun exn ->
         Log.debug (fun l ->
             l "[listen] error while handling %s, continuing. bt: %s"
               (Printexc.to_string exn) (Printexc.get_backtrace ()));
         Lwt.return_unit)

  (* Loop and listen for packets permanently *)
  (* this function has to be tail recursive, since it is called at the
     top level, otherwise memory of received packets and all reachable
     data is never claimed.  take care when modifying, here be
     dragons! *)
  let rec listen t fn =
    match t.active with
    | true ->
      let process () =
        read t >|= function
        | Ok buf              -> Lwt.async (fun () -> safe_apply fn buf) ; Ok ()
        | Error `Disconnected -> t.active <- false ; Error `Disconnected
      in
      process () >>= (function
          | Ok () -> listen t fn
          | Error e -> Lwt.return (Error e))
    | false -> Lwt.return (Ok ())

  (* Transmit a packet from a Cstruct.t *)
  let write t buffer =
    F.write t.flow buffer >|= function
    | Ok () ->
      let len = Cstruct.len buffer in
      t.stats.tx_pkts <- Int32.succ t.stats.tx_pkts;
      t.stats.tx_bytes <- Int64.add t.stats.tx_bytes (Int64.of_int len);
      Ok ()
    | Error e -> Error (`Flow e)

  let writev t = function
    | []     -> Lwt.return (Ok ())
    | [page] -> write t page
    | pages  -> write t @@ Cstruct.concat pages

  let mac t = t.mac

  let get_stats_counters t = t.stats

  let reset_stats_counters t =
    t.stats.rx_bytes <- 0L;
    t.stats.rx_pkts  <- 0l;
    t.stats.tx_bytes <- 0L;
    t.stats.tx_pkts  <- 0l

end
