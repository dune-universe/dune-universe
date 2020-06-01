open! Core_kernel
open! Import

include Accessor.Of_monad (struct
    include Only_in_test

    let apply = `Define_using_bind
  end)
