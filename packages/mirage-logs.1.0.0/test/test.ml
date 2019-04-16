(* Copyright (C) 2016, Thomas Leonard <thomas.leonard@unikernel.com>
   See the README file for details. *)

module Clock = struct
  type error = string
  type t = unit
  type 'a io = 'a Lwt.t
  let connect () : t Lwt.t = Lwt.return_unit
  let disconnect _ = Lwt.return_unit
  let now_d_ps _ = (0, 0L)
  let current_tz_offset_s _ = Some 0
  let period_d_ps _ = None
end

module Logs_reporter = Mirage_logs.Make(Clock)

let src = Logs.Src.create "test" ~doc:"mirage-logs test code"
module Log = (val Logs.src_log src : Logs.LOG)

let noisy_src = Logs.Src.create "noisy" ~doc:"mirage-logs test noisy library"
module Noisy = (val Logs.src_log noisy_src : Logs.LOG)

let src_tag = Logs.Tag.def "src" ~doc:"Source address" Format.pp_print_string
let port_tag = Logs.Tag.def "port" ~doc:"Port number" Format.pp_print_int
let tags ~src ~port = Logs.Tag.(empty |> add src_tag src |> add port_tag port)

let with_pipe fn =
  let r, w = Unix.pipe () in
  let r = Unix.in_channel_of_descr r in
  let w = Unix.out_channel_of_descr w in
  fn ~r ~w;
  close_out w;
  try Alcotest.fail (Printf.sprintf "Unexpected data in pipe: %S" (input_line r))
  with End_of_file ->
  close_in r

let test_console r =
  Log.info (fun f -> f "Simple test");
  Alcotest.(check string) "Simple"
    "1970-01-01 00:00:00 +00:00: INF [test] Simple test" (input_line r);
  Log.warn (fun f -> f ~tags:(tags ~src:"localhost" ~port:7000) "Packet rejected");
  Alcotest.(check string) "Tags"
    "1970-01-01 00:00:00 +00:00: WRN [test] Packet rejected: src=localhost port=7000" (input_line r);
  Log.debug (fun f -> f "Not shown")

let test_no_ring () =
  with_pipe @@ fun ~r ~w ->
  Lwt_main.run begin
    let (>>=) = Lwt.bind in
    Clock.connect () >>= fun clock ->
    Logs.(set_level (Some Info));
    Logs_reporter.(create ~ch:w clock |> run) @@ fun () ->
    test_console r;
    Lwt.return ()
  end

let console_threshold src =
  match Logs.Src.name src with
  | "noisy" -> Logs.Warning
  | _ -> Logs.Info

exception Test_failure

let test_ring () =
  with_pipe @@ fun ~r ~w ->
  try Lwt_main.run begin
    let (>>=) = Lwt.bind in
    Clock.connect () >>= fun clock ->
    Logs.(set_level (Some Info));
    Logs.(set_level (Some Debug));
    Logs_reporter.(create ~ch:w ~ring_size:2 ~console_threshold clock |> run) @@ fun () ->
    test_console r;
    Lwt.fail Test_failure
  end with Test_failure ->
    let expect msg = Alcotest.(check string) "Ring buffer" msg (input_line r) in
    expect "--- Dumping log ring buffer ---";
    expect "1970-01-01 00:00:00 +00:00: WRN [test] Packet rejected: src=localhost port=7000";
    expect "1970-01-01 00:00:00 +00:00: DBG [test] Not shown";
    expect "--- End dump ---"

let test_ring_async () =
  with_pipe @@ fun ~r ~w ->
  let expect msg = Alcotest.(check string) "Ring buffer" msg (input_line r) in
  Lwt_main.run begin
    let (>>=) = Lwt.bind in
    Clock.connect () >>= fun clock ->
    Logs.(set_level (Some Debug));
    Lwt.async_exception_hook := ignore;
    let rep = Logs_reporter.create ~ch:w ~ring_size:2 ~console_threshold clock in
    Logs_reporter.dump_ring rep w;
    expect "--- Dumping log ring buffer ---";
    expect "--- End dump ---";
    Logs_reporter.run rep @@ fun () ->
    test_console r;
    Noisy.info (fun f -> f "From noisy");
    Lwt.async (fun () -> Lwt.fail (Failure "Oops"));
    Lwt.return ()
  end;
  expect "--- Dumping log ring buffer ---";
  expect "1970-01-01 00:00:00 +00:00: DBG [test] Not shown";
  expect "1970-01-01 00:00:00 +00:00: INF [noisy] From noisy";
  expect "--- End dump ---"

let () =
  Alcotest.run "mirage-logs" [
    "Tests", [
      "Logging without a ring", `Quick, test_no_ring;
      "Logging with a ring", `Quick, test_ring;
      "Logging with a ring, async error", `Quick, test_ring_async;
    ];
  ]
