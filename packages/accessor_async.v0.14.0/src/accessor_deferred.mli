open! Core_kernel
open! Async_kernel
open! Import
include Accessor.Monad.S with type 'a t := 'a Deferred.t
module Option : Accessor.Monad.S with type 'a t := 'a option Deferred.t
module Or_error : Accessor.Monad.S with type 'a t := 'a Or_error.t Deferred.t
module Result : Accessor.Monad.S2 with type ('a, 'e) t := ('a, 'e) Result.t Deferred.t
