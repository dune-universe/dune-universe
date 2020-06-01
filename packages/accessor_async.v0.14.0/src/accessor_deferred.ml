open! Core_kernel
open! Async_kernel
open! Import

include Accessor.Of_monad (struct
    include Deferred

    let apply = `Define_using_bind
  end)

module Option = Accessor.Of_monad (struct
    include Deferred.Option

    let apply = `Define_using_bind
  end)

module Or_error = Accessor.Of_monad (struct
    include Deferred.Or_error

    let apply = `Custom apply
  end)

module Result = Accessor.Of_monad2 (struct
    include Deferred.Result

    let apply = `Define_using_bind
  end)
