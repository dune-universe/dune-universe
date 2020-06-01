open! Core_kernel
open! Import

let unreachable =
  [%accessor Accessor.field ~get:Nothing.unreachable_code ~set:Nothing.unreachable_code]
;;
