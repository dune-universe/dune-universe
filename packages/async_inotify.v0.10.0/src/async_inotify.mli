open! Core
open! Async

type t
type file_info = string * Unix.Stats.t

module Event : sig
  type move =
    | Away of string
    | Into of string
    | Move of string * string
  [@@deriving sexp_of]

  type t =
    | Created of string
    | Unlinked of string
    | Modified of string
    | Moved of move
    (* Queue overflow means that you are not consuming events fast enough and just
       lost some of them. This means that some changes to files you want might go
       unnoticed *)
    | Queue_overflow
  [@@deriving sexp_of]

  val to_string : t -> string
end

type modify_event_selector =
  [ `Any_change (** Send a Modified event whenever the contents of the file changes
                    (which can be very often when writing a large file) *)
  | `Closed_writable_fd (** Only send a Modify event when someone with a file descriptor
                            with write permission to that file is closed. There are
                            usually many fewer of these events (for large files),
                            but they come later. *)
  ]

(** [create path] creates an inotify watching path. Returns the inotify type t itself
  and the list of files currently being watched. By default, recursively watches all
  subdirectories of the given path. *)
val create
  :  ?modify_event_selector:modify_event_selector
  -> ?recursive:bool
  -> ?watch_new_dirs:bool
  -> string
  -> (t * file_info list) Deferred.t

(** [create_empty modify_event_selector] creates an inotify that watches nothing
    until [add] or [add_all] is called. *)
val create_empty
  : modify_event_selector:modify_event_selector
  -> t Deferred.t

(** [stop t] stop watching t *)
val stop : t -> unit Deferred.t

(** [add t path] add the path to t to be watched *)
val add : t -> string -> unit Deferred.t

(** [add_all t path] adds the path to t recursively  *)
val add_all : ?skip_dir:(string * Unix.Stats.t -> bool Deferred.t) -> t -> string -> file_info list Deferred.t

(** [remove t path] remove the path from t *)
val remove : t -> string -> unit Deferred.t

(** [stream t] returns a stream of filesystem events *)
val stream : t -> Event.t Stream.t

(** [pipe t] returns a pipe of filesystem events *)
val pipe : t -> Event.t Pipe.Reader.t
