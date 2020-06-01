open! Base
open! Import

(** [Info.t] has a lazy representation. This accessor isn't quite a true isomorphism
    because you can observe whether a [Lazy.t] is already evaluated. *)
val lazy_t : (_, Info.t Lazy.t, Info.t, [< isomorphism ]) Accessor.Simple.t
