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
open Lwt.Infix

let src = Logs.Src.create "netif" ~doc:"Mirage unix network module"
module Log = (val Logs.src_log src : Logs.LOG)

type t = {
  id: string;
  dev: Lwt_unix.file_descr;
  mutable active: bool;
  mac: Macaddr.t;
  mtu : int;
  stats : Mirage_net.stats;
}

let fd t = t.dev

type error = [
  | Mirage_net.Net.error
  | `Partial of string * int * Cstruct.t
  | `Exn of exn
]

let pp_error ppf = function
  | #Mirage_net.Net.error as e -> Mirage_net.Net.pp_error ppf e
  | `Partial (id, len', buffer) ->
    Fmt.pf ppf "netif %s: partial write (%d, expected %d)"
      id len' buffer.Cstruct.len
  | `Exn e -> Fmt.exn ppf e

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
    let mtu = Tuntap.get_mtu devname in
    Log.debug (fun m -> m "plugging into %s with mac %a and mtu %d"
                  devname Macaddr.pp mac mtu);
    let active = true in
    let stats = Mirage_net.Stats.create () in
    let t = { id=devname; dev; active; mac; mtu; stats } in
    Log.info (fun m -> m "connect %s with mac %a" devname Macaddr.pp mac);
    Lwt.return t
  with
  | Failure "tun[open]: Permission denied" [@warning "-52"] ->
    Lwt.fail_with (err_permission_denied devname)
  | exn -> Lwt.fail exn

let disconnect t =
  Log.info (fun m -> m "disconnect %s" t.id);
  t.active <- false;
  Lwt_unix.close t.dev >>= fun () ->
  Tuntap.closetap t.id;
  Lwt.return_unit

(* Input a frame, and block if nothing is available *)
let rec read t buf =
  let process () =
    Lwt.catch (fun () ->
        Lwt_cstruct.read t.dev buf >|= function
        | (-1) -> Error `Continue      (* EAGAIN or EWOULDBLOCK *)
        | 0    -> Error `Disconnected  (* EOF *)
        | len ->
          Mirage_net.Stats.rx t.stats (Int64.of_int len);
          let buf = Cstruct.sub buf 0 len in
          Ok buf)
      (function
        | Unix.Unix_error(Unix.ENXIO, _, _) ->
          Log.err (fun m -> m "[read] device %s is down, stopping" t.id);
          Lwt.return (Error `Disconnected)
        | Lwt.Canceled ->
          Log.err (fun m -> m "[read] user program requested cancellation of listen on %s" t.id);
          Lwt.return (Error `Canceled)
        | exn ->
          Log.err (fun m -> m "[read] error: %s, continuing" (Printexc.to_string exn));
          Lwt.return (Error `Continue))
  in
  process () >>= function
  | Error `Continue -> read t buf
  | Error `Canceled -> Lwt.return (Error `Canceled)
  | Error `Disconnected -> Lwt.return (Error `Disconnected)
  | Ok buf -> Lwt.return (Ok buf)

let safe_apply f x =
  Lwt.catch
    (fun () -> f x)
    (function
      | Out_of_memory -> Lwt.fail Out_of_memory
      | exn ->
        Log.err (fun m -> m "[listen] error while handling %s, continuing. bt: %s"
                    (Printexc.to_string exn) (Printexc.get_backtrace ()));
        Lwt.return_unit)

(* Loop and listen for packets permanently *)
(* this function has to be tail recursive, since it is called at the
   top level, otherwise memory of received packets and all reachable
   data is never claimed.  take care when modifying, here be dragons! *)
let rec listen t ~header_size fn =
  match t.active with
  | true ->
    let buf = Cstruct.create (t.mtu + header_size) in
    let process () =
      read t buf >|= function
      | Ok buf              -> Lwt.async (fun () -> safe_apply fn buf) ; Ok ()
      | Error `Canceled     -> Error `Disconnected
      | Error `Disconnected -> t.active <- false ; Error `Disconnected
    in
    process () >>= (function
        | Ok () -> (listen[@tailcall]) t ~header_size fn
        | Error e -> Lwt.return (Error e))
  | false -> Lwt.return (Ok ())

(* Transmit a packet from a Cstruct.t *)
let write t ~size fillf =
  (* This is the interface to the cruel Lwt world with exceptions, we've to guard *)
  let buf = Cstruct.create size in
  let len = fillf buf in
  if len > size then
    Lwt.return (Error `Invalid_length)
  else
    Lwt.catch (fun () ->
        Lwt_bytes.write t.dev buf.Cstruct.buffer 0 len >|= fun len' ->
        Mirage_net.Stats.tx t.stats (Int64.of_int len);
        if len' <> len then Error (`Partial (t.id, len', buf))
        else Ok ())
      (fun exn -> Lwt.return (Error (`Exn exn)))

let mac t = t.mac

let mtu t = t.mtu

let get_stats_counters t = t.stats

let reset_stats_counters t = Mirage_net.Stats.reset t.stats
