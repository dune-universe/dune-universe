(*
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

open Hvsock
open Lwt.Infix
open Result

module type MAIN = sig
  val run_in_main: (unit -> 'a Lwt.t) -> 'a
end

type ('request, 'response) fn = {
  request: 'request;
  response: 'response Lwt.u;
}

module Make(Main: MAIN) = struct
  type ('request, 'response) t = {
    call: ('request, 'response) fn option -> unit;
  }

  let rec handle_requests blocking_op calls =
    match Main.run_in_main (fun () ->
      Lwt.catch
        (fun () -> Lwt_stream.next calls >>= fun x -> Lwt.return (Some x))
        (fun _ -> Lwt.return None)
      ) with
    | None -> ()
    | Some r ->
      let response =
        try
          Ok (blocking_op r.request)
        with
        | e -> Error e in
      Main.run_in_main (fun () ->
        match response with
        | Ok x -> Lwt.wakeup_later r.response x; Lwt.return_unit
        | Error e -> Lwt.wakeup_later_exn r.response e; Lwt.return_unit
      );
      handle_requests blocking_op calls

  let create blocking_op =
    let calls, call = Lwt_stream.create () in
    let _th = Thread.create (handle_requests blocking_op) calls in
    { call }

  let fn t request =
    let thread, response = Lwt.task () in
    let call = { request; response } in
    t.call (Some call);
    thread

  let destroy t = t.call None
end
