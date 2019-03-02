open! Base
open! Async

module Term : sig
  type t

  val create
    :  ?dispose:bool
    -> ?nosig:bool
    -> ?mouse:bool
    -> ?bpaste:bool
    -> ?reader:Reader.t (** stdin by default *)
    -> ?writer:Writer.t (** stdout by default *)
    -> unit
    -> t Deferred.t

  val refresh : t -> unit Deferred.t
  val image : t -> Notty.image -> unit Deferred.t
  val cursor : t -> (int * int) option -> unit Deferred.t
  val size : t -> int * int

  (** Release the terminal, restoring it to a state where ordinary I/O
     can be performed. *)
  val release : t -> unit Deferred.t

  (** This pipe will automatically be shut down once [release] is
     called, and closing this pipe will asynchronous trigger [release]
     to be called. *)
  val events :
    t -> [ Notty.Unescape.event | `Resize of (int * int) ] Pipe.Reader.t
end
