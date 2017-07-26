open! Async
open! Core
open Core_extended.Std

module Postmark : sig
  type t = {
    from : string;
    time : Time.t;
  } [@@deriving sexp]

  val to_string : t -> string
  val of_string : string -> t
end

module Message : sig
  type t = {
    postmark : Postmark.t;
    email : Email.t;
  } [@@deriving sexp]
  val to_string : t -> string;;
end


module type With_container = sig
  type t

  val t_of_fd : In_channel.t -> t
  val t_of_file : string -> t
  val of_string : string -> t
  val iter_string : t -> f:(string -> unit) -> unit
end

module With_lazy_list : With_container with type t = Message.t Lazy_list.t;;

(**
   Warning: When created from a file descriptor, the resulting mailbox can
   only be traversed once.
*)
module With_seq : With_container with type t = Message.t Lazy_sequence.t;;

module With_pipe : sig
  type t = Message.t Pipe.Reader.t

  val t_of_fd : Fd.t -> t Deferred.t
  val t_of_file : string -> t Deferred.t
  val of_string : string -> t Deferred.t

  (* Mostly for testing purposes *)
  val iter_string : t -> f:(string -> unit Deferred.t) -> unit Deferred.t
end
