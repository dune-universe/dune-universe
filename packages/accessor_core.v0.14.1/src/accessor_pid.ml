open! Core_kernel
open! Import

let int = [%accessor Accessor.isomorphism ~get:Pid.to_int ~construct:Pid.of_int]
