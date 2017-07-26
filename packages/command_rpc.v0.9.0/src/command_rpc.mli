(** Utilities for RPC communication with a child process over stdin and stdout. *)

open! Core
open! Async

(** [Command] is used for setting up an RPC server in the child process.  By default this
    will set up an RPC server, but passing the [-sexp] flag will make it run the
    implementation on a sexp read from stdin instead.  Passing the [-menu] flag
    will cause the command to print out a sexp indicating which RPC names and
    versions are supported.
*)
module Command : sig
  module Invocation : sig
    type t = Sexp | Bin_io of Rpc.Connection.t
  end

  module type T = sig
    type query    [@@deriving of_sexp]
    type response [@@deriving sexp_of]
    val rpc : (query, response) Rpc.Rpc.t
    val implementation : Invocation.t -> query -> response Deferred.t
  end

  module type T_conv = sig
    include Versioned_rpc.Callee_converts.Rpc.S
    val name : string
    val query_of_sexp    : Sexp.t -> query
    val sexp_of_response : response -> Sexp.t
    val implementation : Invocation.t -> query -> response Deferred.t
  end

  module type T_pipe = sig
    type query    [@@deriving of_sexp]
    type response [@@deriving sexp_of]
    type error    [@@deriving sexp_of]
    val rpc : (query, response, error) Rpc.Pipe_rpc.t
    val implementation
      :  Invocation.t
      -> query
      -> (response Pipe.Reader.t, error) Result.t Deferred.t
  end

  type t = [
    | `Plain      of (module T)
    | `Plain_conv of (module T_conv)
    | `Pipe       of (module T_pipe)
  ]

  val create
     : ?heartbeat_config:Rpc.Connection.Heartbeat_config.t
    -> ?log_not_previously_seen_version : (name:string -> int -> unit)
    -> summary                          : string
    -> t list
    -> Command.t
end

module Connection : sig
  type 'a with_connection_args
     = ?heartbeat_config:Rpc.Connection.Heartbeat_config.t
    -> ?propagate_stderr : bool        (* defaults to true *)
    -> ?env              : Process.env (* defaults to [`Extend []] *)
    -> prog              : string
    -> args              : string list
    -> 'a

  (** [create] spawns a child process and returns an RPC connection that operates on the
      child's stdin and stdout.  The child will be killed and reaped when the connection
      is closed.  If [propagate_stderr] is true, the child's stderr will be printed on the
      parent's stderr; otherwise it will be ignored. *)
  val create :
    (unit -> Rpc.Connection.t Or_error.t Deferred.t) with_connection_args

  (** [with_close] spawns a child and connects like [create], calls the function passed in
      on the resulting connection, and then closes the connection and kills the child. *)
  val with_close :
    ((Rpc.Connection.t -> 'a Or_error.t Deferred.t) -> 'a Or_error.t Deferred.t)
    with_connection_args
end
