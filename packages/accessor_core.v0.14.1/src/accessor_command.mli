open! Core_kernel
open! Import
module Param : Accessor.Applicative.S with type 'a t := 'a Command.Param.t
