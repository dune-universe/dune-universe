open! Core_kernel
open! Import

module Generator = Accessor.Of_monad (struct
    include Quickcheck.Generator

    let apply = `Custom apply
  end)
