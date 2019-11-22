
(* This file is free software. See file "license" for more details. *)

type config

val mk_config :
  ?additional_args:(Arg.key * Arg.spec * Arg.doc) list ->
  usage:string ->
  unit ->
  config
(** [mk_config ?additional_args ~usage] will parse command line arguments and
returns a config object to be consumed by {!main}.

    - A connection file can be passed using [--connection-file <file>];
    - A log file through [--log <file>];
    - Individual connection parameters with [--ci-<foo> <bar>];
    - See [--help] for more details;
    - The parameter [additional_args] can contain additional command line arguments.
*)

val main : config:config -> kernel:Client.Kernel.t -> unit Lwt.t
(** [main ~config ~kernel] will open a connection using {!Sockets}, and run the passed
kernel. Run via {!Lwt_main.run} *)
