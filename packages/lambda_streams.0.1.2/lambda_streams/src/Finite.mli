(** Represents the finite version of a {!Lambda_streams.Sync} stream. *)
module Sync : sig
  type 'a input = 'a Signal.t Sync.input

  type 'a output = 'a Sync.output

  val make_output : ('a -> unit) -> 'a output

  val send : 'a -> 'a output -> unit

  val pure : 'a -> 'a input

  val empty : unit -> 'a input

  val from_list : 'a list -> 'a input

  val from_array : 'a array -> 'a input

  (** Pipes an input stream into an output stream. *)
  val pipe : 'a output -> 'a input -> unit

  val map : ('a -> 'b) -> 'a input -> 'b input

  val filter : ('a -> bool) -> 'a input -> 'a input

  val take : int -> 'a input -> 'a input

  (* Takes [n] elements from an infinite stream and converts it to a finite one. *)
  val take' : int -> 'a Sync.input -> 'a input

  val skip : int -> 'a input -> 'a input

  val until : ('a -> bool) -> 'a input -> 'a input

  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b input -> 'a

  val concat : 'a input list -> 'a input

  val flatten : 'a input input -> 'a input

  val to_rev_list : 'a input -> 'a list

  val to_list : 'a input -> 'a list

  val to_array : 'a input -> 'a array
end

(** Represents the finite version of an {!Lambda_streams.Async} stream. *)
module Async : sig
  type 'a t = 'a Signal.t Async.t

  val pure : 'a -> 'a t

  val empty : unit -> 'a t

  val from_list : 'a list -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val filter : ('a -> bool) -> 'a t -> 'a t

  val scan : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b t

  val take : int -> 'a t -> 'a t

  (**
   Takes [n] elements from an infinite stream and converts it to a finite one. Optionally takes an
   output stream ([?close]) to close the connection at {!Signal.EndOfSignal} if the async stream is
   connection-based.
   *)
  val take' : ?close:unit Sync.output -> int -> 'a Async.t -> 'a t
end
