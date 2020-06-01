open! Core_kernel
open! Async_kernel
open! Import
include Accessor.Monad.S with type 'a t := 'a Lazy_deferred.t
