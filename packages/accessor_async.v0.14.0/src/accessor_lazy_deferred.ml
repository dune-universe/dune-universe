open! Core_kernel
open! Async_kernel
open! Import

include Accessor.Of_monad (struct
    include Lazy_deferred

    let apply = `Define_using_bind
  end)
