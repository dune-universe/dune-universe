open! Core_kernel
open! Import

(** These accessors all use [Date.create_exn] when creating new dates, and therefore can
    raise if any fields fail to validate according to that function. *)

val year_exn : (_, int, Date.t, [< field ]) Accessor.Simple.t
val month_exn : (_, Month.t, Date.t, [< field ]) Accessor.Simple.t
val day_exn : (_, int, Date.t, [< field ]) Accessor.Simple.t
