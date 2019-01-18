(*
 * Copyright (C) 2013 Citrix Systems Inc.
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

open Lwt
open OUnit

let ( |> ) a b = b a
let id x = x

let alloc_page () =
  Bigarray.Array1.create Bigarray.char Bigarray.c_layout 4096

let one_request_response () =
  let page = alloc_page () in
  let sring = Ring.Rpc.of_buf ~buf:(Cstruct.of_bigarray page) ~idx_size:1 ~name:"test" in
  let front = Ring.Rpc.Front.init ~sring in
  let back = Ring.Rpc.Back.init ~sring in

  Printf.fprintf stdout "%s\n%!" (Ring.Rpc.Back.to_string back);
  assert_equal ~msg:"more_to_do" ~printer:string_of_bool false (Ring.Rpc.Back.more_to_do back);

  let client = Lwt_ring.Front.init (fun _ -> "unknown") front in
  let server = Lwt_ring.Back.init (fun _ -> "unknown") back in

  let id = () in
  let must_notify = ref false in
  let request_th = Lwt_ring.Front.push_request_and_wait client (fun () -> must_notify := true) (fun _ -> id) in
  assert_equal ~msg:"must_notify" ~printer:string_of_bool true (!must_notify);
  Printf.fprintf stdout "%s\n%!" (Ring.Rpc.Back.to_string back);
  assert_equal ~msg:"more_to_do" ~printer:string_of_bool true (Ring.Rpc.Back.more_to_do back);

  let finished = ref false in
  Ring.Rpc.Back.ack_requests back (fun _ -> finished := true);
  assert_equal ~msg:"ack_requests" ~printer:string_of_bool true (!finished);

  Lwt_ring.Back.push_response server (fun () -> ()) (fun _ -> ());

  Printf.fprintf stdout "%s\n%!" (Ring.Rpc.Back.to_string back);

  let replied = ref false in
  Lwt_ring.Front.poll client (fun _ -> replied := true; id, ());
  assert_equal ~msg:"poll" ~printer:string_of_bool true (!replied);

  assert_equal ~msg:"more_to_do" ~printer:string_of_bool false (Ring.Rpc.Back.more_to_do back);
  Lwt.choose [ Lwt_unix.sleep 5.; request_th ]
  >>= fun () ->
  assert_equal ~msg:"is_sleeping" ~printer:string_of_bool false (Lwt.is_sleeping request_th);
  return ()

let one_request_response () = Lwt_main.run (one_request_response ())

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test shared memory ring code";

  let suite = "ring" >:::
              [
                "one_request_response" >:: one_request_response
              ] in
  run_test_tt ~verbose:!verbose suite
