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

[@@@warning "-52"]
open Result
open Mirage_net

let log fmt = Format.printf ("Netif: " ^^ fmt ^^ "\n%!")

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

type +'a io = 'a Lwt.t

type t = {
  id: string;
  dev: Lwt_unix.file_descr;
  mutable active: bool;
  mutable mac: Macaddr.t;
  stats : Mirage_net.stats;
}

let fd t = t.dev

type error = [
  | Mirage_net.error
  | `Partial of string * int * Cstruct.t
  | `Exn of exn
]

let pp_error ppf = function
  | #Mirage_net.error as e -> Mirage_net.pp_error ppf e
  | `Partial (id, len', buffer) ->
    Fmt.pf ppf "netif %s: partial write (%d, expected %d)"
      id len' buffer.Cstruct.len
  | `Exn e -> Fmt.exn ppf e

let devices = Hashtbl.create 1

let err_permission_denied devname =
  Printf.sprintf
    "Permission denied while opening the %s device. Please re-run using sudo."
    devname

let connect devname =
  try
    Random.self_init ();
    let fd, devname = Tuntap.opentap ~pi:false ~devname () in
    let dev = Lwt_unix.of_unix_file_descr ~blocking:true fd in
    let mac = Macaddr.make_local (fun _ -> Random.int 256) in
    Tuntap.set_up_and_running devname;
    log "plugging into %s with mac %s" devname (Macaddr.to_string mac);
    let active = true in
    let t = {
      id=devname; dev; active; mac;
      stats= { rx_bytes=0L;rx_pkts=0l; tx_bytes=0L; tx_pkts=0l } }
    in
    Hashtbl.add devices devname t;
    log "connect %s" devname;
    Lwt.return t
  with
  | Failure "tun[open]: Permission denied" ->
    Lwt.fail_with (err_permission_denied devname)
  | exn -> Lwt.fail exn

let disconnect t =
  log "disconnect %s" t.id;
  t.active <- false;
  Lwt_unix.close t.dev >>= fun () ->
  Tuntap.closetap t.id;
  Lwt.return_unit

type macaddr = Macaddr.t
type page_aligned_buffer = Io_page.t
type buffer = Cstruct.t

(* Input a frame, and block if nothing is available *)
let rec read t page =
  let buf = Io_page.to_cstruct page in
  let process () =
    Lwt.catch (fun () ->
        Lwt_cstruct.read t.dev buf >|= function
        | (-1) -> Error `Continue      (* EAGAIN or EWOULDBLOCK *)
        | 0    -> Error `Disconnected  (* EOF *)
        | len ->
          t.stats.rx_pkts <- Int32.succ t.stats.rx_pkts;
          t.stats.rx_bytes <- Int64.add t.stats.rx_bytes (Int64.of_int len);
          let buf = Cstruct.sub buf 0 len in
          Ok buf)
      (function
        | Unix.Unix_error(Unix.ENXIO, _, _) ->
          log "[read] device %s is down, stopping" t.id;
          Lwt.return (Error `Disconnected)
        | Lwt.Canceled ->
          log "[read] user program requested cancellation of listen on %s" t.id;
          Lwt.return (Error `Canceled)
        | exn ->
          log "[read] error: %s, continuing" (Printexc.to_string exn);
          Lwt.return (Error `Continue))
  in
  process () >>= function
  | Error `Continue -> read t page
  | Error `Canceled -> Lwt.return (Error `Canceled)
  | Error `Disconnected -> Lwt.return (Error `Disconnected)
  | Ok buf -> Lwt.return (Ok buf)

let safe_apply f x =
  Lwt.catch
    (fun () -> f x)
    (fun exn ->
       log "[listen] error while handling %s, continuing. bt: %s"
         (Printexc.to_string exn) (Printexc.get_backtrace ());
       Lwt.return_unit)

(* Loop and listen for packets permanently *)
(* this function has to be tail recursive, since it is called at the
   top level, otherwise memory of received packets and all reachable
   data is never claimed.  take care when modifying, here be dragons! *)
let rec listen t fn =
  match t.active with
  | true ->
    let page = Io_page.get 1 in
    let process () =
      read t page >|= function
      | Ok buf              -> Lwt.async (fun () -> safe_apply fn buf) ; Ok ()
      | Error `Canceled     -> Error `Disconnected
      | Error `Disconnected -> t.active <- false ; Error `Disconnected
    in
    process () >>= (function
        | Ok () -> (listen[@tailcall]) t fn
        | Error e -> Lwt.return (Error e))
  | false -> Lwt.return (Ok ())

(* Transmit a packet from a Cstruct.t *)
let write t buffer =
  let open Cstruct in
  (* Unfortunately we peek inside the cstruct type here: *)
  (* This is the interface to the cruel Lwt world with exceptions, we've to guard *)
  Lwt.catch (fun () ->
      Lwt_bytes.write t.dev buffer.buffer buffer.off buffer.len >|= fun len' ->
      t.stats.tx_pkts <- Int32.succ t.stats.tx_pkts;
      t.stats.tx_bytes <- Int64.add t.stats.tx_bytes (Int64.of_int buffer.len);
      if len' <> buffer.len then Error (`Partial (t.id, len', buffer))
      else Ok ())
    (fun exn -> Lwt.return (Error (`Exn exn)))


let writev t = function
  | []     -> Lwt.return (Ok ())
  | [page] -> write t page
  | pages  ->
    write t @@ Cstruct.concat pages

let mac t = t.mac

let get_stats_counters t = t.stats

let reset_stats_counters t =
  t.stats.rx_bytes <- 0L;
  t.stats.rx_pkts  <- 0l;
  t.stats.tx_bytes <- 0L;
  t.stats.tx_pkts  <- 0l
