
(* This file is free software. See file "license" for more details. *)

val main :
  ?args:(Arg.key * Arg.spec * Arg.doc) list ->
  usage:string ->
  Client.Kernel.t ->
  unit Lwt.t
(** [main ~usage kernel] will parse command line arguments, open a connection
    using {!Sockets}, and run the [kernel].

    - A connection file can be passed using [--connection-file <file>];
    - A log file through [--log <file>];
    - Individual connection parameters with [--ci-<foo> <bar>];
    - See [--help] for more details;
    - The parameter [args] can contain additional command line arguments.
*)

