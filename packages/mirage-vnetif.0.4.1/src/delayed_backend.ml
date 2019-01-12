(*
 * Copyright (c) 2016 Magnus Skjegstad <magnus@docker.com>
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

module Make
    (TIME : Mirage_time_lwt.S)
    (MCLOCK : Mirage_clock_lwt.MCLOCK)
    (B : Vnetif.BACKEND) =
struct

  type macaddr = B.macaddr
  type 'a io = 'a B.io
  type buffer = B.buffer
  type id = B.id

  type elt = {
    buffers : Cstruct.t list;
    len : int;
    id : id;
  }

  type t = {
    xt : B.t;
    mclock : MCLOCK.t;
    rtt_ns : int64;
    bandwidth_bits : int64;
    mtu : int;
    bdp : int;

    queue : elt Queue.t; (* Add from left, take from right *)
    mutable queue_total_bytes : int;
    mutable queue_next_len : int;
    queue_lock : Lwt_mutex.t;

    queue_not_empty_cond : int Lwt_condition.t;
    queue_not_empty_lock : Lwt_mutex.t;

    mutable last_send_ts : int64;

  }

  let register t =
    B.register t.xt

  let unregister t id =
    B.unregister t.xt id

  let mac t id =
    B.mac t.xt id

  let set_listen_fn t id fn =
    B.set_listen_fn t.xt id fn

  let unregister_and_flush t id =
    B.unregister_and_flush t.xt id

  let ok = Lwt.return (Ok ())

  let writev t id buffers =
    (* Add packet to queue and update counters *)
    let len = List.fold_left (fun a b -> a + Cstruct.len b) 0 buffers in
    let e = { buffers ; len ; id } in
    if (t.queue_total_bytes < t.bdp) then
      (* check outside lock first to avoid taking mutex *)
      Lwt_mutex.with_lock t.queue_lock (fun () ->
          if t.queue_total_bytes < t.bdp then (
            t.queue_total_bytes <- t.queue_total_bytes + e.len;
            t.queue_next_len <- e.len;
            Queue.add e t.queue;
            if Queue.length t.queue = 1 then
              (* queue was empty, signal waiting thread *)
              Lwt_mutex.with_lock t.queue_not_empty_lock (fun () ->
                  Lwt_condition.signal t.queue_not_empty_cond 1;
                  ok
                )
            else ok
          ) else ok (* Send queue full, discard packet to simulate congestion *)
        )
    else ok

  let write t id buffer =
    writev t id [buffer]

  let rec wait_for_token_and_send t =
      (if (t.queue_next_len = 0) then (* no message in queue, wait *)
      begin
          (*Printf.printf "will sleep\n%!";*)
          Lwt_condition.wait ~mutex:t.queue_not_empty_lock t.queue_not_empty_cond >>= fun _x ->
          (* reset tokens by setting timestamp, packet just appeared in queue *)
          t.last_send_ts <- (MCLOCK.elapsed_ns t.mclock);
          (*Printf.printf "awake ts=%Ld\n%!" t.last_send_ts;*)
          Lwt.return_unit
      end
      else begin
          (*Printf.printf "rec ts=%Ld cur=%Ld\n%!" t.last_send_ts (MCLOCK.elapsed_ns t.mclock);*)
          Lwt.return_unit
      end) >>= fun () ->

      let tokens_per_second = Int64.div t.bandwidth_bits 8L in
      let ns_per_token = Int64.div (Duration.of_sec 1) tokens_per_second in
      let ns_since_last_send = Int64.sub (MCLOCK.elapsed_ns t.mclock) t.last_send_ts in

      let tokens =
        (if ns_since_last_send < ns_per_token then
            0L
        else begin
            let total_tokens = (Int64.div ns_since_last_send ns_per_token) in
            if total_tokens > (Int64.of_int t.mtu) then
                Int64.of_int t.mtu
            else begin
                total_tokens
            end
        end)
      in
      (*Printf.printf "tokens=%Ld ns_per_token=%Ld next_len=%d bdp=%d\n%!" tokens ns_per_token t.queue_next_len t.bdp;*)

      (if tokens >= Int64.of_int t.queue_next_len then
          (* We can send *)
          Lwt_mutex.with_lock t.queue_lock (fun () ->
              assert(Queue.is_empty t.queue = false);

              (* get pkt *)
              let cur_pkt_elt = Queue.take t.queue in

              (* decr tokens by setting timestamp *)
              t.last_send_ts <- MCLOCK.elapsed_ns t.mclock;
              t.queue_total_bytes <- t.queue_total_bytes - cur_pkt_elt.len;

              (* get len of next pkt *)
              if Queue.is_empty t.queue then
                  t.queue_next_len <- 0
              else begin
                  let next_pkt_elt = Queue.peek t.queue in
                  t.queue_next_len <- next_pkt_elt.len
              end;

              (* send pkt *)
              (*Printf.printf "send pkt len %d, next len=%d\n%!" cur_pkt_elt.len t.queue_next_len;*)
              B.writev t.xt cur_pkt_elt.id cur_pkt_elt.buffers >>= fun _ ->

              Lwt.return_unit
          )
      else begin
          (* not enough tokens, time to sleep *)
          let time_to_sleep_in_ns current_tokens bytes_to_send =
            let needed_tokens = (Int64.sub bytes_to_send current_tokens) in
            (Int64.mul ns_per_token needed_tokens)
          in
          let ns = time_to_sleep_in_ns tokens (Int64.of_int t.queue_next_len) in
          (*Printf.printf "about to sleep %Ld\n%!" ns;*)
          TIME.sleep_ns ns
      end) >>= fun () ->
      (* TOOD
       * Always sleep after sending packet to regain tokens if a new one is ready
       * If awoken, go to sleep right away as no point in checking if enough tokens *)
      wait_for_token_and_send t

  let create
      (t:B.t) (mclock:MCLOCK.t) ?(rtt_ns=(Duration.of_ms 1))
      ?(bandwidth_bits=1_000_000_000L) ?(mtu=1500) ()
    =
      let bytes_per_second = Int64.div bandwidth_bits 8L in
      let bytes_bandwidth_delay = (Int64.to_float bytes_per_second) *. (Duration.to_f rtt_ns) in
      (*Printf.printf "bdp=%f\n%!" bytes_bandwidth_delay;*)
      let u = { xt = t ; rtt_ns ; bandwidth_bits ; mtu ;
        queue_total_bytes = 0; queue = Queue.create ();
        bdp = int_of_float bytes_bandwidth_delay ; queue_lock = Lwt_mutex.create () ;
        queue_not_empty_cond = Lwt_condition.create () ; queue_not_empty_lock = Lwt_mutex.create () ;
        last_send_ts = MCLOCK.elapsed_ns mclock; mclock; queue_next_len = 0
        } in
      Lwt.async (fun () ->
          Lwt_mutex.lock u.queue_not_empty_lock >>= fun () -> (* must be locked before entering wait_for_token_and_send *)
          wait_for_token_and_send u);
      u

end
