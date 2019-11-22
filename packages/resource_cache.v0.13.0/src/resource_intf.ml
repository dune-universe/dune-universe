open! Core_kernel
open! Async_kernel
open! Import

module type Simple = sig
  module Key : sig
    type t [@@deriving sexp_of]

    include Comparable.S_plain with type t := t
    include Hashable.S_plain with type t := t
  end

  module Common_args : T

  type t

  val open_ : Key.t -> Common_args.t -> t Or_error.t Deferred.t
  val close : t -> unit Deferred.t
end

module type S = sig
  module Key : sig
    type t [@@deriving sexp_of]

    include Comparable.S_plain with type t := t
    include Hashable.S_plain with type t := t
  end

  module Common_args : T

  type t

  val open_ : Key.t -> Common_args.t -> t Deferred.Or_error.t
  val close : t -> unit Deferred.t
  val close_finished : t -> unit Deferred.t

  (** [has_close_started t] should return [true] iff [close t] has been called, even if
      [close_finished] has not been determined. *)
  val has_close_started : t -> bool
end

module type S_wrapped = sig
  include S

  type resource

  val underlying : t -> resource
end

module type Resource = sig
  module type S = S
  module type S_wrapped = S_wrapped
  module type Simple = Simple

  (** Wrap a resource that does not natively support a [has_close_started] operation
      in a simple record to add such tracking. *)
  module Make_simple (R : Simple) :
    S_wrapped
    with module Key = R.Key
     and module Common_args = R.Common_args
     and type resource = R.t
end
