open! Base
open! Import

(** Accesses [()] iff the boolean is [true]. *)
val true_ : (_, unit, bool, [< variant ]) Accessor.Simple.t

(** Accesses [()] iff the boolean is [false]. *)
val false_ : (_, unit, bool, [< variant ]) Accessor.Simple.t

(** Access a boolean as its inverse. *)
val negated : (_, bool, bool, [< isomorphism ]) Accessor.Simple.t
