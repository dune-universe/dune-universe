open! Core_kernel
open! Import

(** You can access anything if you put your mind to it. Nothing is impossible! *)
val unreachable : ('i -> _ -> _, 'i -> Nothing.t -> Nothing.t, [< field ]) Accessor.t
