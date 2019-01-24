(*---------------------------------------------------------------------------
  Copyright (c) 2018 Zach Shipko. All rights reserved. Distributed under the
  ISC license, see terms at the end of the file. %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)
open Lwt.Infix
open Resp_unix

module Server =
  Server.Make
    (Resp_server.Auth.String)
    (struct
      type data = (string, string) Hashtbl.t
    end)

include Util.Make (Server)

let client, pid =
  Lwt_main.run
    ( match Unix.fork () with
    | -1 ->
      Printf.eprintf "Unable to fork process";
      exit 1
    | 0 ->
      let server = `TCP (`Port 1234) in
      let data = Hashtbl.create 8 in
      let server =
        Server.create ~commands (Conduit_lwt_unix.default_ctx, server) data
      in
      Server.start server >|= fun () -> exit 0
    | pid ->
      Unix.sleep 1;
      let addr = Ipaddr.of_string_exn "127.0.0.1" in
      let params =
        (Conduit_lwt_unix.default_ctx, `TCP (`IP addr, `Port 1234))
      in
      Client.connect params >|= fun client -> (client, pid) )

let invalid_response () = Alcotest.fail "Invalid response type"

let test_set _ () =
  Client.run_s client [|"set"; "abc"; "123"|]
  >|= function
  | `String s ->
    Alcotest.(check string) "set OK" s "OK"
  | _ ->
    invalid_response ()

let test_get _ () =
  Client.run_s client [|"get"; "abc"|]
  >|= function
  | `Bulk s ->
    Alcotest.(check string) "Value of abc" s "123"
  | _ ->
    invalid_response ()

let basic =
  [ Alcotest_lwt.test_case "Set" `Quick test_set
  ; Alcotest_lwt.test_case "Get" `Quick test_get ]

let () =
  Alcotest.run ~and_exit:false "Resp_unix" [("basic", basic)];
  Unix.kill pid Sys.sigint

(*---------------------------------------------------------------------------
  Copyright (c) 2018 Zach Shipko

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
  REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
  AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
  INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
  LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
  OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
  PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
