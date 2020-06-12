open! Base
open! Import

(** [Error.t] and [Info.t] are actually the same thing, though they are not exposed as
    type equal. This isomorphism is essentially an equality. *)
val info : (_, Info.t, Error.t, [< isomorphism ]) Accessor.Simple.t

(** [Error.t] has a lazy representation. This accessor isn't quite a true isomorphism
    because you can observe whether a [Lazy.t] is already evaluated. *)
val lazy_t : (_, Error.t Lazy.t, Error.t, [< isomorphism ]) Accessor.Simple.t
