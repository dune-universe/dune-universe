(*
 * Copyright (c) 2015 Unikernel Systems
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

let _ =
  match Launchd.(error_to_msg (activate_socket "Listener")) with
  | Ok fds ->
    while true do
      let ready_fds, _, _ = Unix.select fds [] [] (-1.) in
      List.iter (fun fd ->
        let client, _ = Unix.accept fd in
        let message = Bytes.of_string "Hello there!\n" in
        let (_: int) = Unix.write client message 0 (Bytes.length message) in
        Unix.close client
      ) ready_fds
    done
  | Error (`Msg m) ->
    Printf.fprintf stderr "%s\n%!" m;
    exit (-1)
