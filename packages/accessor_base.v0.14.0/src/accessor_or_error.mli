open! Base
open! Import

(** [ok] and [error] are the same as [Result.ok] and [Result.error], but specialized for
    [Or_error]. *)

val ok : ('i -> 'a -> 'b, 'i -> 'a Or_error.t -> 'b Or_error.t, [< variant ]) Accessor.t
val error : (_, Error.t, 'a Or_error.t, [< variant ]) Accessor.Simple.t

include Accessor.Monad.S with type 'a t := 'a Or_error.t
