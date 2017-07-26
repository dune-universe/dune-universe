(** For communicating a dynamically chosen TCP port from a child process to its parent.

    This is used to fork+exec a child process that will create create a TCP server that
    listens to a dynamically chosen port, and to make the port number available in the
    parent process once the child process is listening on the port.

    Here is the intended usage:

    1. The parent [create]s a [Dynamic_port_writer.t] value together with a deferred that
    will eventually be determined with the port assigned to the child process by the OS.

    2. The parent communicates the [Dynamic_port_writer.t] value to a child it has
    spawned.  This can happen in a number of ways,

    - via the child's command line using [to_string] and either [arg] or [of_string].
    - via a config file using "with sexp"
    - over the wire using "with bin_io"

    3. The child calls [Tcp.Server.create] with the value returned by [where_to_listen].

    Once the server created in step (3) is listening on its OS-assigned port, the parent's
    deferred obtained in step (1) will soon become determined with the value of the port.

    Code for the parent process would look something like:

    {[
      Dynamic_port_writer.create ()
      >>= fun (dynamic_port_writer, port_d) ->
      Unix.fork_exec ~prog
        ~argv:([ prog ]
               @ Dynamic_port_writer.flag_args dynamic_port_writer
               @ [ ... other args ... ])
        ()
      >>= fun _child_pid ->
      port_d
      >>= fun r ->
      let `Port port = ok_exn r in
      Tcp.connect (Tcp.to_host_and_port "localhost" port)
      >>= fun (_, reader, writer) ->
      ...
    ]}

    Code for the [Command.t] for the child process would look something like:

    {[
      Command.basic
        ~summary:"child"
        (Command.Spec.(empty +> Dynamic_port_writer.flag))
        (fun dynamic_port_writer () ->
           ...
           Tcp.Server.create
               (Dynamic_port_writer.where_to_listen dynamic_port_writer)
               (fun _ reader writer -> ...))
    ]} *)

open! Core
open! Import

type t [@@deriving bin_io, sexp]

include Stringable with type t := t

val create : unit -> (t * [`Port of int] Or_error.t Deferred.t) Deferred.t

val where_to_listen : t -> (Socket.Address.Inet.t, int) Tcp.Where_to_listen.t

(** For use with [Command]. *)
val arg  : t Command.Spec.Arg_type.t

(** One can pass a [t] from parent to child by including [flag_args t] in the command-line
    arguments and using [flag] in the [Command.t] in the child. *)
val flag : t Command.Spec.param
val flag_args : t -> string list
