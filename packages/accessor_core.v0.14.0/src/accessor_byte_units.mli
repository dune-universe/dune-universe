open! Core_kernel
open! Import

(** All of these accessors will raise if a bounds check fails. The expected bounds are
    documented next to the corresponding conversion functions in [Core_kernel.Byte_units].
*)

val bytes : (_, int, Byte_units.t, [< isomorphism ]) Accessor.Simple.t
val kilobytes : (_, float, Byte_units.t, [< isomorphism ]) Accessor.Simple.t
val megabytes : (_, float, Byte_units.t, [< isomorphism ]) Accessor.Simple.t
val gigabytes : (_, float, Byte_units.t, [< isomorphism ]) Accessor.Simple.t
val terabytes : (_, float, Byte_units.t, [< isomorphism ]) Accessor.Simple.t
val petabytes : (_, float, Byte_units.t, [< isomorphism ]) Accessor.Simple.t
val exabytes : (_, float, Byte_units.t, [< isomorphism ]) Accessor.Simple.t
val words : (_, float, Byte_units.t, [< isomorphism ]) Accessor.Simple.t
