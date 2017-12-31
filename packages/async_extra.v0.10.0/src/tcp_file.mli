(** Access to on-disk files in parallel with serving them over a TCP connection. *)

open! Core
open! Import

module Server : sig
  module File : sig
    type t
  end

  (** [serve ~port ()] must be called before clients will be able to connect.  Sets up a
      bound tcp socket on port that will be used to serve files to clients.  The unit
      Deferred will be filled when the server is ready to receive clients.

      The [auth] function will be called once for every client connection.  If it returns
      false the client will be disconnected immediately.  Further details of [auth] can be
      found in the documentation for [Rpc.serve].

      Once [serve] has been called it is a mistake (and will raise) if it is ever called
      again.
  *)
  val serve
    :  auth:(Socket.Address.Inet.t -> bool)
    -> Tcp.Where_to_listen.inet
    -> Tcp.Server.inet Deferred.t

  (** [open_file filename] open a file for writing.  The filename given should
      be a real path on the server, and will create a real file there *)
  val open_file
    :  ?append     : bool  (** default is [false] *)
    -> ?dos_format : bool  (** default is [false] *)
    -> string
    -> File.t Deferred.t

  (** [serve_existing_static_file filename] adds [filename] to the list of files
      that can be accessed via the Client module.  As indicated in the name,
      this file must already exist and must not grow or change.  When a client
      requests the file it will be served from beginning to end. *)
  val serve_existing_static_file : string -> unit Deferred.t

  (** [stop_serving t] stops serving the file t to clients.  Calling this
      function will not close the file if it is open.  (A possible use of this
      function is if you want to delete the file on the sending side.) *)
  val stop_serving : File.t -> unit

  (** [close t] closes the file t for writing.  If [stop_serving] is false
      (default is [true]) the file will be left on disk and will still be served
      to clients on a create request. *)
  val close : ?stop_serving:bool (** default is [true] *) -> File.t -> unit Deferred.t

  (** [write_message t msg] write [msg] to [t].  [msg] is assumed to contain no
      newlines except possibly at the end.  A newline will be added to the end
      in the file if it is not present.  The message may be transmitted to
      clients with or without a newline.  The string passed to write_message is copied
      and so may be freely modified once write_message returns. *)
  val write_message : File.t -> string -> unit

  (** [schedule_message t msg] is write, but the message is taken from the
      provided bigstring.  Once a bigstring has been given to this function it is a
      mistake to ever modify the bigstring in the future.  This is because it will take an
      unknown amount of time to deliver the message to all connected clients *)
  val schedule_message : File.t -> Bigstring.t -> unit

  (** [write_sexp t sexp] writes a Sexp.t as a single message *)
  val write_sexp : File.t -> Sexp.t -> unit

  (** [flushed t] becomes determined only once all messages written to [t] have been
      written to disk. *)
  val flushed : File.t -> unit Deferred.t

  (** [with_file filename ~f] opens filename and runs [f], passing the resultant
      [t].  When the deferred returned by [f] is determined, [t] will be
      closed. *)
  val with_file
    :  ?append : bool  (** default is [false] *)
    -> string
    -> f       : (File.t -> 'a Deferred.t)
    -> 'a Deferred.t

  (** [monitor t] returns a monitor which will listen to errors arising from the
      internal writer used by [t]. *)
  val writer_monitor : File.t -> (Monitor.t, [ `This_is_a_static_file ]) Result.t

  (** [debug_snapshot ()] returns an s-expression containing details of the current
      state of the Tcp_file server. *)
  val debug_snapshot : unit -> Sexp.t
end

module Client : sig
  type t

  module Error : sig
    type t =
      | File_not_found of string
      | Unknown        of string
    [@@deriving sexp]

    val to_string : t -> string
  end

  module Message : sig
    type t =
      | String    of string
      | Bigstring of Bigstring.t
    [@@deriving bin_io]

    val length        : t -> int

    (** None is returned in cases where the message cannot fit into a string (original
        message was a very large Bigstring *)
    val to_string     : t -> string option
    val to_string_exn : t -> string
    val to_bigstring  : t -> Bigstring.t
  end

  module Response : sig
    type t = (Message.t, Error.t) Result.t
  end

  val connect : host:string -> port:int -> (t, Exn.t) Result.t Deferred.t

  (** Pipes delivered by read/tail will be closed. *)
  val disconnect : t -> unit Deferred.t

  (** [read ?client_pushes_back t filename] provides a pipe that will be filled with
      messages from [filename] starting from the beginning, and continuing until the
      server calls [unlink] or [close].  The client can indicate that it is no longer
      interested by calling [Pipe.close_reader].  See [Rpc.Pipe_rpc.create] for the
      definition on ?client_pushes_back and its implications. Passing the
      client_pushes_back argument will have no detrimental effect on a server that
      is delivering from a file. If the client caught up, effectively tailing the file,
      then the server will enqueue the data in memory until the client catches up, or gets
      disconnected.
  *)
  val read
    :  ?client_pushes_back : unit
    -> t
    -> string
    -> Response.t Pipe.Reader.t Deferred.t

  (** [tail ?client_pushes_back t filename] same as [read], but delivers messages starting
      at some unspecified point near the current end of the file and continuing until the
      server calls [unlink] or [close]. The client can indicate that it is no longer
      interested by calling [Pipe.close_reader]. *)
  val tail
    :  ?client_pushes_back : unit
    -> t
    -> string
    -> Response.t Pipe.Reader.t Deferred.t
end
