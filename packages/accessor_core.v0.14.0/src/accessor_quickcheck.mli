open! Core_kernel
open! Import
module Generator : Accessor.Monad.S with type 'a t := 'a Quickcheck.Generator.t
