open! Base
open! Import

include sig
  type ('ok, 'err) t =
    | Ok of 'ok
    | Error of 'err
  [@@deriving accessors]
end
with type ('ok, 'err) t := ('ok, 'err) Result.t

include Accessor.Monad.S2 with type ('a, 'e) t := ('a, 'e) Result.t
