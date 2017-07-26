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

open Result
open Mirage_net
open Lwt.Infix

let src = Logs.Src.create "vnetif" ~doc:"in-memory network interface"
module Log = (val Logs.src_log src : Logs.LOG)

module type BACKEND = sig
    type 'a io = 'a Lwt.t
    type buffer = Cstruct.t
    type id = int
    type macaddr = Macaddr.t
    type t

    val register : t -> [ `Ok of id | `Error of error ]
    val unregister : t -> id -> unit io
    val mac : t -> id -> macaddr
    val write : t -> id -> buffer -> (unit, error) result io
    val writev : t -> id -> buffer list -> (unit, error) result io
    val set_listen_fn : t -> id -> (buffer -> unit io) -> unit
    val unregister_and_flush : t -> id -> unit io
end

module Make (B : BACKEND) = struct
  type page_aligned_buffer = Io_page.t
  type buffer = B.buffer
  type error = Mirage_net.error
  let pp_error = Mirage_net.pp_error
  type macaddr = B.macaddr
  type +'a io = 'a Lwt.t

  type t = {
    id : B.id;
    backend : B.t;
    mutable wake_listener : unit Lwt.u option;
    size_limit : int option;
    stats : stats;
  }

  let connect ?size_limit backend =
      match (B.register backend) with
      | `Error _ -> Lwt.fail_with "vnetif: error while registering to backend"
      | `Ok id ->
          let stats = { rx_bytes = 0L ; rx_pkts = 0l; tx_bytes = 0L; tx_pkts = 0l } in
          let t = { id; size_limit; backend; stats; wake_listener=None } in
          Lwt.return t

  let disconnect t =
      B.unregister t.backend t.id >>= fun () ->
      match t.wake_listener with
      | None -> Lwt.return_unit
      | Some e -> (Lwt.wakeup e ()); Lwt.return_unit

  let write t buffer =
    let send buffer =
      t.stats.tx_bytes <- Int64.add t.stats.tx_bytes (Int64.of_int (Cstruct.len buffer));
      t.stats.tx_pkts <- Int32.succ t.stats.tx_pkts;
      B.write t.backend t.id buffer
    in
    match t.size_limit with
    | None ->Log.debug (fun f -> f "no size limit, sending");send buffer
    | Some limit ->
      Log.debug (fun f -> f "checking packet size %d against size limit %d" (Cstruct.len buffer) limit);
      assert (limit >= (Cstruct.len buffer));
      send buffer

  let writev t buffers =
    let send buffers =
      let total_len = (List.fold_left (fun a b -> a + (Cstruct.len b)) 0 buffers) in
      t.stats.tx_bytes <- Int64.add t.stats.tx_bytes (Int64.of_int total_len);
      t.stats.tx_pkts <- Int32.succ t.stats.tx_pkts; (* assembled to single packet *)
      B.writev t.backend t.id buffers
    in
    match t.size_limit with
    | None ->Log.debug (fun f -> f "no size limit, sending");send buffers
    | Some limit ->
      Log.debug (fun f -> f "checking packet size %d against size limit %d" (Cstruct.lenv buffers) limit);
      assert (limit >= (Cstruct.lenv buffers));
      send buffers

  let listen t fn =
    let listener t fn buf =
      t.stats.rx_bytes <- Int64.add (Int64.of_int (Cstruct.len buf)) (t.stats.rx_bytes);
      t.stats.rx_pkts <- Int32.succ t.stats.rx_pkts;
      fn buf
    in
    B.set_listen_fn t.backend t.id (listener t fn);
    let task, waker = MProf.Trace.named_task "Netif.listen" in
    t.wake_listener <- (Some waker);
    task >|= fun () ->
    Ok ()

  let mac t =
    B.mac t.backend t.id

  let get_stats_counters t =
    t.stats

  let reset_stats_counters t =
    t.stats.rx_bytes <- 0L;
    t.stats.rx_pkts  <- 0l;
    t.stats.tx_bytes <- 0L;
    t.stats.tx_pkts  <- 0l

end
