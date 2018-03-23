open! Core
open! Async

(** Erlang style mailboxes built on top of async streams *)
type 'a t [@@deriving sexp_of]

module Filter : sig
  type ('a, 'b) t = {
    name   : string;
    select : 'a -> 'b option;
  }

  val create : string -> ('a -> 'b option) -> ('a, 'b) t

  (* arrows inspired interface for composing filters *)

  (** an always matching filter from a function *)
  val arr : ('a -> 'b) -> ('a, 'b) t

  (** Compose two filters such that both are applied to the same value,
      and their corresponding results are paired in a tuple. This filter
      will fail if any of the two argument filters fails. *)
  val ( &&& ) : ('a, 'b) t -> ('a, 'c) t -> ('a, ('b * 'c)) t

  (** Compose two filters **)
  val ( >>> ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t

  val to_predicate : ('a, 'b) t -> ('a -> bool)
end

val create :
  ?to_sexp:('a -> Sexp.t)
  -> (unit -> 'a option Deferred.t)
  -> 'a t

val of_stream :
  ?to_sexp:('a -> Sexp.t)
  -> 'a Stream.t
  -> 'a t

val of_pipe :
  ?to_sexp:('a -> Sexp.t)
  -> 'a Pipe.Reader.t
  -> 'a t

(** [receive t ~filter ~postcond] apply [postcond] to the messages picked by [filter].
    Return if [postcond] returns true, otherwise keep trying until [timeout] becomes
    determined, which will raise an exception.

    If this returns successfully, the remaining data in the mailbox will be:

    - the list of messages that did not pass the filter, in the order received,
      iff [swallow] is false.
    - the list of messages that did not pass the filter AND arrived after the
      last message that did pass the filter, in the order received, iff
      [swallow] is true OR if no messages passed the filter.
*)
val receive
  :  ?debug:string
  -> ?timeout:unit Deferred.t
  -> ?swallow:bool
  -> 'a t
  -> filter: ('a, 'b) Filter.t
  -> postcond:('b list -> bool)
  -> 'b list Deferred.t

(* returns elements from the mailbox in ascending order by arrival time *)
val peek : 'a t -> ('a, 'b) Filter.t -> 'b list

(** [zero t f] asserts that there are exactly zero matching messages. *)
val zero : ?debug:string -> 'a t -> ('a, 'b) Filter.t -> unit

(** [one t f] run receive, asserting that there is exactly one matching message. *)
val one
  :  ?debug:string
  -> ?timeout:unit Deferred.t
  -> ?swallow:bool
  -> 'a t
  -> ('a, 'b) Filter.t
  -> 'b Deferred.t

(** [two t f] run receive, asserting that there are exactly two matching messages. *)
val two
  :  ?debug:string
  -> ?timeout:unit Deferred.t
  -> ?swallow:bool
  -> 'a t
  -> ('a, 'b) Filter.t
  -> ('b * 'b) Deferred.t

(** [many t n f] run receive, asserting that there are exactly [n] matching messages. *)
val many
  :  ?debug:string
  -> ?timeout:unit Deferred.t
  -> ?swallow:bool
  -> 'a t
  -> int
  -> ('a, 'b) Filter.t
  -> 'b list Deferred.t

(** [not_empty t f] run receive, asserting that there is at least one matching message. *)
val not_empty
  :  ?debug:string
  -> ?timeout:unit Deferred.t
  -> 'a t
  -> ('a, 'b) Filter.t
  -> 'b list Deferred.t

(** [clear t] wipes out all previously received messages matching the [to_remove]
    predicate. If [to_remove] is not provided, wipes out all the messages.

    Immediately after calling [clear t], [zero t f] succeeds for any [f].
*)
val clear : ?to_remove:('a -> bool) -> 'a t -> unit

(** [check_clear t] - Ok if the mailbox is empty, descriptive error if the mailbox
    has any messages *)
val check_clear : _ t -> unit Or_error.t

(** [filter t f] removes all elements currently in [t] that satisfy [f].
    Future arrivals are unaffected.
*)
val filter : 'a t -> ('a, _) Filter.t -> unit
