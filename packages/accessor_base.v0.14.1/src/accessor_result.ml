open! Base
open! Import

type ('ok, 'err) t = ('ok, 'err) Result.t =
  | Ok of 'ok
  | Error of 'err
[@@deriving accessors]

include Accessor.Of_monad2 (struct
    include Result

    let apply = `Define_using_bind
  end)
