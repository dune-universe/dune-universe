open! Core_kernel
open! Import

include Accessor.Of_monad (struct
    include Blang

    let apply = `Define_using_bind
  end)
