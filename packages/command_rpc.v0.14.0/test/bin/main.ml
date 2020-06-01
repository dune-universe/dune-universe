open! Core
open! Async

let pipe_conv_command =
  Command.async
    ~summary:""
    (let%map_open.Command () = return ()
     and serve = Command_rpc.Command.Expert.param ()
     and min_version = flag "-min-version" (required int) ~doc:"min supported version"
     and max_version = flag "-max-version" (required int) ~doc:"max supported version" in
     fun () ->
       serve
         [ `Pipe_conv
             (Command_rpc_test_protocol.Heartbeat_pipe_rpc.server
                ~min_version
                ~max_version
              :> (module Command_rpc.Command.T_pipe_conv))
         ])
;;

let pipe_direct_command =
  Command.async
    ~summary:""
    (let%map_open.Command () = return ()
     and serve = Command_rpc.Command.Expert.param () in
     fun () ->
       serve
         [ `Implementations
             Command_rpc_test_protocol.Heartbeat_pipe_direct_rpc.implementations
         ])
;;

let state_conv_command =
  Command.async
    ~summary:""
    (let%map_open.Command () = return ()
     and serve = Command_rpc.Command.Expert.param ()
     and min_version = flag "-min-version" (required int) ~doc:"min supported version"
     and max_version = flag "-max-version" (required int) ~doc:"max supported version" in
     fun () ->
       serve
         [ `Implementations
             (Command_rpc_test_protocol.Heartbeat_state_rpc.server
                ~min_version
                ~max_version)
         ])
;;

let streamable_state_conv_command =
  Command.async
    ~summary:""
    (let%map_open.Command () = return ()
     and serve = Command_rpc.Command.Expert.param ()
     and min_version = flag "-min-version" (required int) ~doc:"min supported version"
     and max_version = flag "-max-version" (required int) ~doc:"max supported version" in
     fun () ->
       serve
         [ `Implementations
             (Command_rpc_test_protocol.Heartbeat_streamable_state_rpc.server
                ~min_version
                ~max_version)
         ])
;;

let () =
  Command.group
    ~summary:""
    [ "pipe-conv", pipe_conv_command
    ; "pipe-direct", pipe_direct_command
    ; "state-conv", state_conv_command
    ; "streamable-state-conv", streamable_state_conv_command
    ]
  |> Command.run
;;
