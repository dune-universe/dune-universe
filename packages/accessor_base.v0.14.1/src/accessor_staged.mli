open! Base
open! Import

val staged
  : ('i -> 'a Staged.t -> 'b Staged.t, 'i -> 'a -> 'b, [< isomorphism ]) Accessor.t

val unstaged
  : ('i -> 'a -> 'b, 'i -> 'a Staged.t -> 'b Staged.t, [< isomorphism ]) Accessor.t
