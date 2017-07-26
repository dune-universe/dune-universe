(*
 * Copyright (C) 2013 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
 * INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt
open Printf

let fd1, _ = Unix.pipe ()
let fd2, _ = Unix.pipe ()

let run test =
  Lwt_main.run (test ())

let test_open () =
  Netif_fd.connect fd1 >>= fun _t ->
  printf "connected\n%!";
  Lwt.return_unit

let test_close () =
  Netif_fd.connect fd1 >>= fun t ->
  printf "connected\n%!";
  Netif_fd.disconnect t >>= fun () ->
  printf "disconnected\n%!";
  Lwt.return_unit

let test_write () =
  Netif_fd.connect fd2 >>= fun t ->
  let data = Cstruct.create 4096 in
  Netif_fd.writev t [ data ] >>= fun _t ->
  Netif_fd.writev t [ data ; (Cstruct.create 14) ] >>= fun _t ->
  Lwt.return_unit

let suite : Alcotest.test_case list = [
  "connect", `Quick, (fun () -> run test_open) ;
  "disconnect", `Quick, (fun () -> run test_close);
  "write", `Quick, (fun () -> run test_write);
]

let _ =
  Alcotest.run "mirage-net-unix" [ "tests", suite ]
