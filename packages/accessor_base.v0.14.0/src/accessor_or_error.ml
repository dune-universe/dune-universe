open! Base
open! Import

let ok = Accessor_result.ok
let error = Accessor_result.error

include Accessor.Of_monad (struct
    include Or_error

    let apply = `Custom apply
  end)
