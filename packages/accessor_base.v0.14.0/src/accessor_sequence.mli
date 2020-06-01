open! Base
open! Import
include Accessor.Monad.S with type 'a t := 'a Sequence.t

module Generator :
  Accessor.Monad.S2 with type ('a, 'b) t := ('a, 'b) Sequence.Generator.t
