(*
 * Copyright (C) 2015 David Scott <dave.scott@unikernel.com>
 * Copyright (C) 2016 Docker Inc
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
 *
 *)

let src =
  let src = Logs.Src.create "flow_lwt_hvsock" ~doc:"AF_HYPERV flow" in
  Logs.Src.set_level src (Some Logs.Debug);
  src

module Log = (val Logs.src_log src : Logs.LOG)

module Make(Time: Mirage_time_lwt.S)(Fn: S.FN)(Socket_family: Hvsock.Af_common.S) = struct

module Blocking_socket = Socket_family
module Socket = Socket.Make(Time)(Fn)(Socket_family)
module RWBuffering = Buffering.Make(Fn)(Socket_family)

type 'a io = 'a Lwt.t

type buffer = Cstruct.t

type error = [ `Unix of Unix.error ]
let pp_error ppf (`Unix e) = Fmt.string ppf (Unix.error_message e)
type write_error = [ Mirage_flow.write_error | error ]
let pp_write_error ppf = function
  |#Mirage_flow.write_error as e -> Mirage_flow.pp_write_error ppf e
  |#error as e -> pp_error ppf e

type flow = {
  socket: Socket.t;
  flow: RWBuffering.flow;
  mutable closed: bool;
  mutable shutdown_write: bool;
}

let connect ?(message_size = 8192) ?(buffer_size = 262144) socket =
  let fd = match Socket.to_fd socket with Some x -> x | None -> assert false in
  let flow = RWBuffering.connect ~message_size ~buffer_size fd in
  let closed = false in
  let shutdown_write = false in
  { socket; flow; closed; shutdown_write } 

let close t =
  Log.debug (fun f -> f "FLOW.close called");
  match t.closed with
  | false ->
    t.closed <- true;
    RWBuffering.close t.flow
  | true ->
    Lwt.return ()

let shutdown_read _t =
  (* We don't care about shutdown_read. We care about shutdown_write because
     we want to send an EOF to the remote and still receive a response. *)
  Log.debug (fun f -> f "FLOW.shutdown_read called and ignored");
  Lwt.return_unit

let shutdown_write t =
  (* When we shutdown_write we still expect buffered data to be flushed. *)
  Log.debug (fun f -> f "FLOW.shutdown_write called");
  match t.shutdown_write || t.closed with
  | true ->
    Lwt.return ()
  | false ->
    Log.debug (fun f -> f "shutting down writer thread");
    t.shutdown_write <- true;
    RWBuffering.shutdown_write t.flow

  let read t = RWBuffering.read t.flow

  let read_into _flow _buffer =
    (* Can we drop this function altogether? *)
    Log.err (fun f -> f "read_into not implemented");
    failwith "not implemented read_into"

  let writev t bufs = RWBuffering.writev t.flow bufs
  let write t buf = RWBuffering.write t.flow buf
end
