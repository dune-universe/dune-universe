open! Core
open! Async

let pipe_conv_command =
  Command.async
    ~summary:""
    (let%map_open.Command.Let_syntax () = return ()
     and serve = Command_rpc.Command.Expert.param ()
     and min_version = flag "-min-version" (required int) ~doc:"min supported version"
     and max_version = flag "-max-version" (required int) ~doc:"max supported version" in
     fun () ->
       serve
         [ `Pipe_conv
             ( Command_rpc_test_protocol.Heartbeat_pipe_rpc.server
                 ~min_version
                 ~max_version
               :> (module Command_rpc.Command.T_pipe_conv) )
         ])
;;

let () = Command.group ~summary:"" [ "pipe-conv", pipe_conv_command ] |> Command.run
