open! Core
open! Async
open! Import

module Versions = struct
  type t =
    { client : int
    ; server_min : int
    ; server_max : int
    }
end

let test (versions : Versions.t) =
  let n = 3 in
  let get_all_responses conn =
    let rpc =
      Command_rpc_test_protocol.Heartbeat_streamable_state_rpc.client
        ~version:versions.client
    in
    let%bind.Deferred.Or_error (), updates =
      Streamable.State_rpc.dispatch rpc (Command_rpc.Connection.rpc_connection conn) n
    in
    let%map updates = Pipe.to_list updates in
    Ok updates
  in
  let%bind result =
    Command_rpc.Connection.with_close
      get_all_responses
      ~prog:"../bin/main.exe"
      ~args:
        ([ [ "streamable-state-conv" ]
         ; [ "-min-version"; Int.to_string versions.server_min ]
         ; [ "-max-version"; Int.to_string versions.server_max ]
         ]
         |> List.concat)
  in
  show_raise ~hide_positions:true (fun () ->
    [%test_eq: unit list] (ok_exn result) (List.init n ~f:ignore));
  Deferred.unit
;;

let%expect_test "client is up to date" =
  let%bind () = test { client = 2; server_min = 1; server_max = 2 } in
  [%expect {|
    "did not raise" |}]
;;

let%expect_test "client is acceptably behind" =
  let%bind () = test { client = 1; server_min = 1; server_max = 2 } in
  [%expect {|
    "did not raise" |}]
;;

let%expect_test "client is too far behind" =
  let%bind () = test { client = 0; server_min = 1; server_max = 2 } in
  [%expect
    {|
    (raised (
      (rpc_error (Unimplemented_rpc heartbeat-streamable-state (Version 0)))
      (connection_description <created-directly>)
      (rpc_tag                heartbeat-streamable-state)
      (rpc_version            0))) |}]
;;

let%expect_test "client is ahead" =
  let%bind () = test { client = 3; server_min = 1; server_max = 2 } in
  [%expect
    {|
    (raised (
      (rpc_error (Unimplemented_rpc heartbeat-streamable-state (Version 3)))
      (connection_description <created-directly>)
      (rpc_tag                heartbeat-streamable-state)
      (rpc_version            3))) |}]
;;
