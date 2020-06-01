open! Base
open! Import

include Accessor.Of_monad (struct
    include Sequence

    let apply = `Define_using_bind
  end)

module Generator = Accessor.Of_monad2 (struct
    include Sequence.Generator

    let apply = `Define_using_bind
  end)
