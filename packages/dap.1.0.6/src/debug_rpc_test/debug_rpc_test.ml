open Lwt_expect

module Foo_command = struct
  let type_ = "foo"

  module Arguments = struct
    type t = {
      foo : string;
    }
    [@@deriving yojson]
  end

  module Result = struct
    type t = {
      bar : string;
    }
    [@@deriving yojson]
  end
end

module Null_command = struct
  let type_ = "null"

  module Arguments = struct
    type t = unit
    [@@deriving yojson]
  end

  module Result = struct
    type t = unit
    [@@deriving yojson]
  end
end

module Int_event = struct
  let type_ = "int"

  module Payload = struct
    type t = int
    [@@deriving yojson]
  end
end

let create_rpc_pair () =
  let (server_in, client_out) = Lwt_unix.pipe () in
  let (client_in, server_out) = Lwt_unix.pipe () in
  let client_rpc = Debug_rpc.create ~in_:(client_in |> Lwt_io.of_fd ~mode:Lwt_io.input) ~out:(client_out |> Lwt_io.of_fd ~mode:Lwt_io.output) () in
  let server_rpc = Debug_rpc.create ~in_:(server_in |> Lwt_io.of_fd ~mode:Lwt_io.input) ~out:(server_out |> Lwt_io.of_fd ~mode:Lwt_io.output) () in
  client_rpc, server_rpc

let lwt_reporter () =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    Fmt.with_buffer ~like b,
    fun () -> let m = Buffer.contents b in Buffer.reset b; m
  in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
  let reporter = Logs_fmt.reporter ~app ~dst () in
  let report src level ~over k msgf =
    let k () =
      let write () = match level with
      | Logs.App -> Lwt_io.write Lwt_io.stdout (app_flush ())
      | _ -> Lwt_io.write Lwt_io.stderr (dst_flush ())
      in
      let unblock () = over (); Lwt.return_unit in
      Lwt.finalize write unblock |> Lwt.ignore_result;
      k ()
    in
    reporter.Logs.report src level ~over:(fun () -> ()) k msgf;
  in
  { Logs.report = report }

let%expect_test "exec_command" =
  let client_rpc, server_rpc = create_rpc_pair () in
  let handle_foo_command (arg : Foo_command.Arguments.t) =
    Format.printf "foo: %s\n" arg.foo;
    Lwt.return Foo_command.Result.{bar = "2000"}
  in
  Debug_rpc.set_command_handler server_rpc (module Foo_command) handle_foo_command;
  Lwt.async (fun () -> (
    Debug_rpc.start server_rpc
  ));
  Lwt.async (fun () -> (
    Debug_rpc.start client_rpc
  ));
  let%lwt res = Debug_rpc.exec_command client_rpc (module Foo_command) Foo_command.Arguments.{foo = "1000"} in
  Format.printf "bar: %s\n" res.bar;
  [%expect {|
    foo: 1000
    bar: 2000 |}]

let%expect_test "cancellation" =
  let client_rpc, server_rpc = create_rpc_pair () in
  let handle_null_command (_arg : Null_command.Arguments.t) =
    Format.printf "1\n";
    Lwt.catch (fun () -> (
      let%lwt () = Lwt_unix.sleep 1.0 in
      Format.printf "2\n";
      Lwt.return ()
    )) (fun err -> (
      let () = match err with
      | Lwt.Canceled -> Format.printf "6\n"
      | _ -> Format.printf "7\n";
      in
      Lwt.return ()
    ));
  in
  Debug_rpc.set_command_handler server_rpc (module Null_command) handle_null_command;
  Lwt.async (fun () -> (
    Debug_rpc.start server_rpc
  ));
  Lwt.async (fun () -> (
    Debug_rpc.start client_rpc
  ));
  Format.printf "3\n";
  let p = Debug_rpc.exec_command client_rpc (module Null_command) () in
  Lwt.async (fun () -> (
    Lwt.cancel p;
    Lwt.return ();
  ));
  let%lwt () =
    try%lwt
      let%lwt () = p in
      Format.printf "4\n";
      Lwt.return ()
    with
      | Lwt.Canceled -> (
        Format.printf "5\n";
        Lwt.return ()
      )
      | _ -> (
        Format.printf "6\n";
        Lwt.return ()
      )
  in
  [%expect {|
    3
    1
    2
    5 |}]

let%expect_test "send_event" =
  let client_rpc, server_rpc = create_rpc_pair () in
  Lwt.async (fun () -> (
    Debug_rpc.start server_rpc
  ));
  Lwt.async (fun () -> (
    Debug_rpc.start client_rpc
  ));
  Lwt.async (fun () -> (
    let%lwt () = Debug_rpc.send_event client_rpc (module Int_event) 1 in
    let%lwt () = Debug_rpc.send_event client_rpc (module Int_event) 2 in
    let%lwt () = Debug_rpc.send_event client_rpc (module Int_event) 3 in
    let%lwt () = Lwt_unix.sleep 0.0 in
    let%lwt () = Debug_rpc.send_event client_rpc (module Int_event) 4 in
    Lwt.return ()
  ));
  let int_e = Debug_rpc.event server_rpc (module Int_event) |> Lwt_react.E.to_stream in
  let%lwt i = Lwt_stream.next int_e in
  Format.printf "%d\n" i;
  let%lwt i = Lwt_stream.next int_e in
  Format.printf "%d\n" i;
  let%lwt i = Lwt_stream.next int_e in
  Format.printf "%d\n" i;
  let%lwt () = Lwt_unix.sleep 0.0 in
  let%lwt i = Lwt_stream.next int_e in
  Format.printf "%d\n" i;
  [%expect {|
    1
    2
    3
    4 |}]
