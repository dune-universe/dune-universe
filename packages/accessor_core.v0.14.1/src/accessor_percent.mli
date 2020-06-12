open! Core_kernel
open! Import

(** Access a percent as a float. *)
val mult : (_, float, Percent.t, [< isomorphism ]) Accessor.Simple.t

(** Access a percent as a float multiplied by 100. This is not quite well-behaved, due to
    potential rounding error. *)
val percentage : (_, float, Percent.t, [< isomorphism ]) Accessor.Simple.t

(** Access a percent as a float multiplied by 10_000. This is not quite well-behaved, due
    to potential rounding error. *)
val bp : (_, float, Percent.t, [< isomorphism ]) Accessor.Simple.t

(** [scaled] is not a well-behaved accessor, because it does not necessarily round trip. *)
val scaled : float -> (_, Percent.t, Percent.t, [< isomorphism ]) Accessor.Simple.t
