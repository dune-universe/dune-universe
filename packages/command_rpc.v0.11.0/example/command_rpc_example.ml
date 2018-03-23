open Core
open Async

module V1 = struct
  type query = int [@@deriving bin_io, sexp]
  type response = int [@@deriving bin_io, sexp]
end

module V2 = struct
  type query = int * int [@@deriving bin_io, sexp]
  type response = int [@@deriving bin_io, sexp]
end

module V3 = V2

(* Protocol definition. Used by both caller and callee *)
module Protocol = struct
  include Versioned_rpc.Caller_converts.Rpc.Make(struct
      let name = "command-rpc-demo"
      type query = V3.query
      type response = V3.response
    end)

  module V1 = Register(struct
      let version = 1
      type query = int [@@deriving bin_io]
      type response = int [@@deriving bin_io]

      let query_of_model (x, y) =
        begin if x <> 10 then
            failwith "v1 only supports adding 10"
        end;
        y
      ;;

      let model_of_response = Fn.id
    end)

  module V2 = Register(struct
      let version = 2
      type query = int * int [@@deriving bin_io]
      type response = int [@@deriving bin_io]

      let query_of_model = Fn.id
      let model_of_response = Fn.id
    end)

  module V3 = Register(struct
      let version = 3
      type query = int * int [@@deriving bin_io]
      type response = int [@@deriving bin_io]

      let query_of_model = Fn.id
      let model_of_response = Fn.id
    end)
end

(* Callee implementations *)
module Impl_V1 = struct
  type query = V1.query [@@deriving sexp]
  type response = V1.response [@@deriving sexp]

  let rpc = Protocol.V1.rpc

  let implementation (_: Command_rpc.Command.Invocation.t) x =
    return (x + 10)
  ;;
end

module Impl_V2 = struct
  type query = V2.query [@@deriving sexp]
  type response = V2.response [@@deriving sexp]

  let rpc = Protocol.V2.rpc

  let implementation (_: Command_rpc.Command.Invocation.t) (x, y) =
    return (x + y)
  ;;
end

module Impl_V3 = struct
  type query = V3.query [@@deriving sexp]
  type response = V3.response [@@deriving sexp]

  let rpc = Protocol.V3.rpc

  let implementation (_: Command_rpc.Command.Invocation.t) (x, y) =
    Core.printf "hello world via Core\n%!";
    Async.printf "hello world via Async\n";
    Writer.flushed (Lazy.force Writer.stdout)
    >>= fun () ->
    Writer.write (Lazy.force Writer.stderr) "hello world via Async stderr\n";
    Writer.flushed (Lazy.force Writer.stderr)
    >>= fun () ->
    Unix.fork_exec ~prog:"echo" ~argv:["echo"; "hello world via fork&exec"] ()
    >>= Unix.waitpid
    >>| Unix.Exit_or_signal.or_error
    >>| Or_error.ok_exn
    >>= fun () ->
    return (x + y)

end


(* In a real use, you would probably only have one command, which only supported the most
   recent version. *)

let v1_implementation_command =
  Command_rpc.Command.create
    ~summary:"rpc interface"
    [ `Plain (module Impl_V1) ]
;;

let v2_implementation_command =
  Command_rpc.Command.create
    ~summary:"rpc interface"
    [ `Plain (module Impl_V2) ]
;;

let v3_implementation_command =
  Command_rpc.Command.create
    ~summary:"rpc interface"
    [ `Plain (module Impl_V3) ]
;;

module Spawn_sleep_1000_and_print_its_pid_to_fd_7 = struct

  type query = unit [@@deriving sexp, bin_io]
  type response = unit [@@deriving sexp, bin_io]

  let rpc =
    Rpc.Rpc.create
      ~name:"spawn-sleep-1000-and-print-its-pid-to-fd-7"
      ~version:1
      ~bin_query
      ~bin_response

  let implementation (_: Command_rpc.Command.Invocation.t) () =
    Process.run_expect_no_output_exn ~prog:"bash" ~args:[
      "-euo"; "pipefail"; "-c"; "sleep 1000 >/dev/null 2>/dev/null & echo $! >&7"] ()

end
;;

let spawn_sleep_1000_and_print_its_pid_to_fd_7_command =
  Command_rpc.Command.create
    ~summary:"rpc interface"
    [ `Plain (module Spawn_sleep_1000_and_print_its_pid_to_fd_7) ]
;;

(* Caller example *)
let caller_command =
  let version_flag =
    Command.Arg_type.of_alist_exn
      [ "v1", "v1-implementation"
      ; "v2", "v2-implementation"
      ; "v3", "v3-implementation"
      ]
  in
  let method_flag =
    Command.Arg_type.Export.sexp_conv
      [%of_sexp: [`normal | `via_bash | `binary of string]]
  in
  let open Command.Let_syntax in
  Command.async_or_error
    ~summary:"test"
    [%map_open
      let version = flag "-version" ~doc:" rpc version to use" (required version_flag)
      and method_ =
        flag "-method" (optional_with_default `normal method_flag) ~doc:" how to execute"
      and x = anon ("x" %: int)
      and y = anon ("y" %: int)
      in
      fun () ->
        let prog, args =
          match method_ with
          | `normal         -> Sys.argv.(0) , [version]
          | `via_bash       -> "bash" , ["-c"; Sys.argv.(0) ^ " " ^ version]
          | `binary binary  -> binary , [version]
        in
        Command_rpc.Connection.with_close ~prog ~args
          (fun connection ->
             Versioned_rpc.Connection_with_menu.create connection
             >>=? fun connection_with_menu ->
             Protocol.dispatch_multi connection_with_menu (x, y)
             >>|? fun result ->
             printf "result: %d\n" result
          )
    ]
;;

let custom_process_create =
  let open Command.Let_syntax in
  Command.async_or_error
    ~summary:"test"
    [%map_open
      let () = return () in
      fun () ->
        (* This example is kind of silly but one can imagine a library for creating
           processes from binaries stored on some server (exe-server). *)
        let process_create ~prog ~args ?env () =
          Process.create ?env ~prog:"/proc/self/exe" ~args:(prog :: args) ()
        in
        let prog = "v1-implementation" in
        let args = [] in
        Command_rpc.Connection.with_close ~process_create ~prog ~args
          (fun connection ->
             Versioned_rpc.Connection_with_menu.create connection
             >>=? fun connection_with_menu ->
             Protocol.dispatch_multi connection_with_menu (10, 1)
             >>|? fun result ->
             printf "result: %d\n" result
          )
    ]
;;

let () =
  let command =
    Command.group
      ~summary:"command_rpc demo"
      [ "v1-implementation", v1_implementation_command;
        "v2-implementation", v2_implementation_command;
        "v3-implementation", v3_implementation_command;
        "spawn-sleep-1000-and-print-its-pid-to-fd-7",
        spawn_sleep_1000_and_print_its_pid_to_fd_7_command;
        "caller", caller_command;
        "custom-process-create", custom_process_create
      ]
  in
  Command.run command
;;
