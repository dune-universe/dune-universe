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

let log fmt = Format.printf ("Netif: " ^^ fmt ^^ "\n%!")

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

type +'a io = 'a Lwt.t

type id = Id of int

let pp_id ppf (Id id) = Fmt.pf ppf "fd:%d" id

type t = {
  id: id;
  dev: Lwt_unix.file_descr;
  mutable active: bool;
  mutable mac: Macaddr.t;
  stats : Mirage_net.stats;
}

type error = [
  | Mirage_net.error
  | `Partial of id * int * Cstruct.t
  | `Exn of exn
]

let pp_error ppf = function
  | #Mirage_net.error as e -> Mirage_net.pp_error ppf e
  | `Partial (id, len', buffer) ->
    Fmt.pf ppf "netif %a: partial write (%d, expected %d)"
      pp_id id len' buffer.Cstruct.len
  | `Exn e -> Fmt.exn ppf e

let devices = Hashtbl.create 1

let int_of_fd (x: Unix.file_descr) : int = Obj.magic x

let connect ?mac fd =
  Random.self_init ();
  let id = Id (int_of_fd fd) in
  let dev = Lwt_unix.of_unix_file_descr ~blocking:true fd in
  let mac = match mac with
    | None   -> Macaddr.make_local (fun _ -> Random.int 256)
    | Some m -> m
  in
  log "plugging into %a with mac %s" pp_id id (Macaddr.to_string mac);
  let active = true in
  let t = {
    id; dev; active; mac;
    stats= { rx_bytes=0L;rx_pkts=0l; tx_bytes=0L; tx_pkts=0l } }
  in
  Hashtbl.add devices id t;
  log "connect %a" pp_id id;
  Lwt.return t

let disconnect t =
  log "disconnect %a" pp_id t.id;
  t.active <- false;
  Lwt_unix.close t.dev >>= fun () ->
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
          log "[read] device %a is down, stopping" pp_id t.id;
          Lwt.return (Error `Disconnected)
        | exn ->
          log "[read] error: %s, continuing" (Printexc.to_string exn);
          Lwt.return (Error `Continue))
  in
  process () >>= function
  | Error `Continue -> read t page
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
      | Error `Disconnected -> t.active <- false ; Error `Disconnected
    in
    process () >>= (function
        | Ok () -> listen t fn
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
  | pages  -> write t @@ Cstruct.concat pages

let mac t = t.mac

let get_stats_counters t = t.stats

let reset_stats_counters t =
  t.stats.rx_bytes <- 0L;
  t.stats.rx_pkts  <- 0l;
  t.stats.tx_bytes <- 0L;
  t.stats.tx_pkts  <- 0l
