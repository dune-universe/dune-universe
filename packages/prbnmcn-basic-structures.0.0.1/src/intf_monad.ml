(** Module types related to monads *)

(** Module type of a plain monad *)
module type Monad = sig
  (** ['a t] is the type of computations of type ['a] *)
  type 'a t

  (** ['a res] is the outcome of running a computation of type ['a] *)
  type 'a res

  (** [return x] injects a value [x] as a computation *)
  val return : 'a -> 'a t

  (** Monadic bind *)
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  (** Functorial map *)
  val map : 'a t -> ('a -> 'b) -> 'b t

  (** Running a monadic computation *)
  val run : 'a t -> 'a res

  module Infix : sig
    (** Alias to [bind]. *)
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

    (** Alias to [map]. *)
    val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t

    (** Alias to [bind]. *)
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

    (** Convenience reexport of [return]. *)
    val return : 'a -> 'a t
  end
end

(** Monad dedicated to handling computations spanning several stages *)
module type Codegen_monad = sig
  (** ['a m] is the type of generated programs of type ['a] *)
  type 'a m

  (** ['a t] is the type of computations of type ['a] *)
  type 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val run : 'a m t -> 'a m

  val lift1 : ('a -> 'b) -> 'a t -> 'b t

  val lift2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val lift3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t

  module Infix : sig
    (** Alias to [bind]. *)
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

    (** Alias to [bind]. *)
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( >> ) : unit m t -> (unit -> 'a t) -> 'a t

    (** [let*! x = m in f] uses the target language binding construct
        to bind [x] to [m] and passes x to the continuation [f].

        In contrast to [let* x = return m in f], this has the effect
        of not duplicating [m] at each use site. *)
    val ( let*! ) : 'a m -> ('a m -> 'b t) -> 'b t

    val ( >>! ) : unit m -> (unit -> 'b t) -> 'b t
  end
end
