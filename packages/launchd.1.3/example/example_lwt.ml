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

open Lwt.Infix

let t =
  Lwt_launchd.activate_socket "Listener"
  >>= fun x ->
  match Launchd.error_to_msg x with
  | Ok fds ->
    Lwt_list.iter_p (fun fd ->
      let loop () =
        Lwt_unix.accept fd
        >>= fun (client, _) ->
        let message = Bytes.of_string "Hello there!\n" in
        Lwt_unix.write client message 0 (Bytes.length message)
        >>= fun (_: int) ->
        Lwt_unix.close client in
      loop ()
    ) fds
  | Error (`Msg m) ->
    Printf.fprintf stderr "%s\n%!" m;
    exit (-1)

let () = Lwt_main.run t
