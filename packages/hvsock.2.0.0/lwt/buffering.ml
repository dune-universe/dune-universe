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

open Lwt.Infix

let src =
  let src = Logs.Src.create "flow_lwt_hvsock" ~doc:"AF_HYPERV flow" in
  Logs.Src.set_level src (Some Logs.Debug);
  src

module Log = (val Logs.src_log src : Logs.LOG)

module Histogram = struct
  type t = (int, int) Hashtbl.t
  (** A table of <bucket> to <count> *)

  let create () = Hashtbl.create 7

  let add t size =
    let existing =
      if Hashtbl.mem t size
      then Hashtbl.find t size
      else 0 in
    Hashtbl.replace t size (existing + 1)

end

module Make(Fn: S.FN)(RW: Hvsock.Af_common.S) = struct

type 'a io = 'a Lwt.t

type buffer = Cstruct.t

type error = [ `Unix of Unix.error ]
let pp_error ppf (`Unix e) = Fmt.string ppf (Unix.error_message e)
type write_error = [ Mirage_flow.write_error | error ]
let pp_write_error ppf = function
  |#Mirage_flow.write_error as e -> Mirage_flow.pp_write_error ppf e
  |#error as e -> pp_error ppf e

type flow = {
  fd: RW.t;
  read_buffers_max: int;
  read_max: int;
  mutable read_buffers: Cstruct.t list;
  mutable read_buffers_len: int;
  read_buffers_m: Mutex.t;
  read_buffers_c: Condition.t;
  read_histogram: Histogram.t;
  mutable write_buffers: Cstruct.t list;
  mutable write_buffers_len: int;
  write_buffers_m: Mutex.t;
  write_buffers_c: Condition.t;
  write_buffers_max: int;
  write_max: int;
  mutable write_flushed: bool;
  write_histogram: Histogram.t;
  mutable closed: bool;
  mutable shutdown_read: bool;
  mutable read_thread_exit: bool;
  mutable shutdown_write: bool;
  mutable shutdown_write_complete: bool;
  mutable write_error: bool;
}

let connect ?(message_size = 8192) ?(buffer_size = 262144) fd =
  let read_buffers_max = buffer_size in
  let read_max = message_size in
  let read_buffers = [] in
  let read_buffers_len = 0 in
  let read_buffers_m = Mutex.create () in
  let read_buffers_c = Condition.create () in
  let read_histogram = Histogram.create () in
  let write_buffers = [] in
  let write_buffers_len = 0 in
  let write_buffers_m = Mutex.create () in
  let write_buffers_c = Condition.create () in
  let write_buffers_max = buffer_size in
  let write_max = message_size in
  let write_flushed = false in
  let write_histogram = Histogram.create () in
  let closed = false in
  let shutdown_read = false in
  let read_thread_exit = false in
  let shutdown_write = false in
  let shutdown_write_complete = false in
  let write_error = false in

  let t = { fd; read_buffers_max; read_max; read_buffers; read_buffers_len;
    read_buffers_m; read_buffers_c; write_buffers; write_buffers_len;
    write_buffers_m; write_buffers_c; closed; shutdown_read; read_thread_exit;
    shutdown_write; shutdown_write_complete;
    write_buffers_max; write_max; write_flushed; write_error;
    read_histogram; write_histogram } in


  let write_thread () =
    let get_buffers () =
      Mutex.lock write_buffers_m;
      while t.write_buffers = [] && not t.shutdown_write do
        Condition.wait write_buffers_c write_buffers_m
      done;
      let result = t.write_buffers in
      t.write_buffers <- [];
      t.write_buffers_len <- 0;
      if t.shutdown_write then t.shutdown_write_complete <- true;
      Mutex.unlock write_buffers_m;
      Condition.broadcast write_buffers_c;
      List.rev result  in
    try
      while not t.closed && not t.shutdown_write_complete do
        Log.debug (fun f -> f "write_thread get_buffers()");
        let buffers = get_buffers () in
        let rec loop remaining =
          if Cstructs.len remaining = 0 then begin
            Log.debug (fun f -> f "write_thread EOF");
          end else begin
            let to_write = min t.write_max (Cstructs.len remaining) in
            Histogram.add t.write_histogram to_write;
            let buf = Cstructs.sub remaining 0 to_write in
            Log.debug (fun f -> f "write_thread writing %d" (Cstructs.len buf));
            let n = RW.writev fd buf in
            Log.debug (fun f -> f "write_thread wrote %d" n);
            loop @@ Cstructs.shift remaining n
          end in
        loop buffers
      done;
      Log.debug (fun f -> f "write_thread write_flushed <- true");
      Mutex.lock write_buffers_m;
      t.write_flushed <- true;
      Condition.broadcast write_buffers_c;
      Mutex.unlock write_buffers_m
    with e ->
      Log.debug (fun f -> f "write_thread caught %s" (Printexc.to_string e));
      Log.err (fun f -> f "Flow write_thread caught: %s" (Printexc.to_string e));
      Mutex.lock write_buffers_m;
      t.write_error <- true;
      t.write_flushed <- true;
      Condition.broadcast write_buffers_c;
      Mutex.unlock write_buffers_m
    in
  let _ = Thread.create write_thread () in
  let read_thread () =
    let get_buffer () =
      Mutex.lock t.read_buffers_m;
      while t.read_buffers_len = t.read_buffers_max do
        Condition.wait t.read_buffers_c t.read_buffers_m
      done;
      let allowed = t.read_buffers_max - t.read_buffers_len in
      let buf = Cstruct.create allowed in
      Mutex.unlock t.read_buffers_m;
      buf in
    try
      while not t.closed && not t.shutdown_read do
        let buffer = get_buffer () in
        let rec loop remaining =
          if Cstruct.len remaining = 0 then begin
            Log.debug (fun f -> f "read_thread EOF")
          end else begin
            let to_read = min t.read_max (Cstruct.len remaining) in
            let buf = Cstruct.sub remaining 0 to_read in
            Histogram.add t.read_histogram to_read;
            Log.debug (fun f -> f "read_thread reading...");
            let n = RW.read_into fd buf in
            Log.debug (fun f -> f "read_thread read %d" n);
            let data = Cstruct.sub remaining 0 n in
            Mutex.lock t.read_buffers_m;
            t.read_buffers <- t.read_buffers @ [ data ];
            t.read_buffers_len <- t.read_buffers_len + (Cstruct.len data);
            Condition.broadcast t.read_buffers_c;
            Mutex.unlock t.read_buffers_m;
            if n = 0 then begin
              Log.debug (fun f -> f "read_thread read length 0");
              Log.err (fun f -> f "Read of length 0 from AF_HVSOCK");
              raise End_of_file
            end else loop @@ Cstruct.shift remaining n
          end in
        loop buffer
      done
    with e ->
      Log.err (fun f -> f "Flow read_thread caught: %s" (Printexc.to_string e));
      Log.debug (fun f -> f "read_thread read_thread_exit <- true");
      Mutex.lock t.read_buffers_m;
      t.read_thread_exit <- true;
      Condition.broadcast read_buffers_c;
      Mutex.unlock t.read_buffers_m
    in
  let _ = Thread.create read_thread () in
  t

let detach f x =
  let fn = Fn.create f in
  Lwt.finalize
    (fun () -> Fn.fn fn x)
    (fun () -> Fn.destroy fn; Lwt.return_unit)

let wait_write_flush t =
  Log.debug (fun f -> f "wait_write_flush");
  Mutex.lock t.write_buffers_m;
  while not t.write_flushed do
    Condition.wait t.write_buffers_c t.write_buffers_m
  done;
  Mutex.unlock t.write_buffers_m

let close t =
  Log.debug (fun f -> f "FLOW.close called");
  match t.closed with
  | false ->
    Mutex.lock t.write_buffers_m;
    t.closed <- true;
    Condition.broadcast t.write_buffers_c;
    Mutex.unlock t.write_buffers_m;
    detach wait_write_flush t
    >>= fun () ->
    detach RW.close t.fd
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
    Mutex.lock t.write_buffers_m;
    t.shutdown_write <- true;
    Condition.broadcast t.write_buffers_c;
    Mutex.unlock t.write_buffers_m;
    detach wait_write_flush t
    >>= fun () ->
    detach RW.shutdown_write t.fd

(* Block until either data is available or EOF *)
let wait_for_data_or_eof flow n =
  Mutex.lock flow.read_buffers_m;
  while flow.read_buffers_len < n && not flow.read_thread_exit do
    Condition.wait flow.read_buffers_c flow.read_buffers_m;
  done;
  Mutex.unlock flow.read_buffers_m

let read flow =
  (* On shutdown_read we drop buffered data. *)
  if flow.closed || flow.shutdown_read then Lwt.return (Ok `Eof)
  else begin
    Mutex.lock flow.read_buffers_m;
    let take () =
      match flow.read_buffers with
      | result :: rest ->
        flow.read_buffers <- rest;
        flow.read_buffers_len <- flow.read_buffers_len - (Cstruct.len result);
        Condition.broadcast flow.read_buffers_c;
        `Data result
      | [] ->
        `Eof in
    if flow.read_buffers = [] then begin
      Mutex.unlock flow.read_buffers_m;
      detach (wait_for_data_or_eof flow) 1 >|= fun () ->
      (* Assume for now there's only one reader so no-one will steal the data *)
      Mutex.lock flow.read_buffers_m;
      let result = take () in
      Mutex.unlock flow.read_buffers_m;
      Ok result
    end else begin
      let result = take () in
      Mutex.unlock flow.read_buffers_m;
      Lwt.return (Ok result)
    end
  end

let read_into _flow _buffer =
  (* Can we drop this function altogether? *)
  Log.err (fun f -> f "read_into not implemented");
  failwith "not implemented read_into"

let wait_for_space flow n =
  Mutex.lock flow.write_buffers_m;
  while (flow.write_buffers_len + n) > flow.write_buffers_max do
    Condition.wait flow.write_buffers_c flow.write_buffers_m;
  done;
  Mutex.unlock flow.write_buffers_m

let writev flow bufs =
  if flow.closed || flow.shutdown_write || flow.write_error then Lwt.return (Error `Closed) else begin
    let len = List.fold_left (+) 0 (List.map Cstruct.len bufs) in
    Mutex.lock flow.write_buffers_m;
    let put () =
      flow.write_buffers <- (List.rev bufs) @ flow.write_buffers;
      flow.write_buffers_len <- flow.write_buffers_len + len;
      Condition.broadcast flow.write_buffers_c in
    if flow.write_buffers_len + len > flow.write_buffers_max then begin
      Mutex.unlock flow.write_buffers_m;
      detach (wait_for_space flow) len >|= fun () ->
      (* Assume for now there's only one writer so no-one will steal the space *)
      Mutex.lock flow.write_buffers_m;
      put ();
      Mutex.unlock flow.write_buffers_m;
      Ok ()
    end else begin
      put ();
      Mutex.unlock flow.write_buffers_m;
      Lwt.return (Ok ())
    end
  end

let write flow buf = writev flow [ buf ]
end
