open! Core_kernel
open! Import

val mapper
  :  (unit -> 'a -> 'b, unit -> 'at -> 'bt, [> mapper ]) Accessor.t
  -> 'at
  -> ('bt -> Sexp.t)
  -> f:('a -> 'b)
  -> unit

val mapperi
  :  ('i -> 'a -> 'b, unit -> 'at -> 'bt, [> mapper ]) Accessor.t
  -> 'at
  -> ('bt -> Sexp.t)
  -> f:('i Accessor.Index.t -> 'a -> 'b)
  -> unit

val many_getter
  :  (unit -> 'a -> 'b, unit -> 'at -> 'bt, [> many_getter ]) Accessor.t
  -> 'at
  -> ('a -> Sexp.t)
  -> unit

val many_getteri
  :  ('i -> 'a -> 'b, unit -> 'at -> 'bt, [> many_getter ]) Accessor.t
  -> 'at
  -> ('i -> Sexp.t)
  -> ('a -> Sexp.t)
  -> unit

val many
  :  (unit -> 'a -> 'b, unit -> 'at -> 'bt, [> many ]) Accessor.t
  -> 'at
  -> ('a -> Sexp.t)
  -> ('bt -> Sexp.t)
  -> f:('a -> 'b)
  -> unit

val manyi
  :  ('i -> 'a -> 'b, unit -> 'at -> 'bt, [> many ]) Accessor.t
  -> 'at
  -> ('i -> Sexp.t)
  -> ('a -> Sexp.t)
  -> ('bt -> Sexp.t)
  -> f:('i Accessor.Index.t -> 'a -> 'b)
  -> unit

val getter
  :  (unit -> 'a -> 'b, unit -> 'at -> 'bt, [> getter ]) Accessor.t
  -> 'at
  -> ('a -> Sexp.t)
  -> unit

val getteri
  :  ('i -> 'a -> 'b, unit -> 'at -> 'bt, [> getter ]) Accessor.t
  -> 'at
  -> ('i -> Sexp.t)
  -> ('a -> Sexp.t)
  -> unit

val field
  :  (unit -> 'a -> 'b, unit -> 'at -> 'bt, [> field ]) Accessor.t
  -> 'at
  -> ('a -> Sexp.t)
  -> ('bt -> Sexp.t)
  -> f:('a -> 'b)
  -> unit

val fieldi
  :  ('i -> 'a -> 'b, unit -> 'at -> 'bt, [> field ]) Accessor.t
  -> 'at
  -> ('i -> Sexp.t)
  -> ('a -> Sexp.t)
  -> ('bt -> Sexp.t)
  -> f:('i Accessor.Index.t -> 'a -> 'b)
  -> unit

val isomorphism
  :  (unit -> 'a -> 'b, unit -> 'at -> 'bt, [> isomorphism ]) Accessor.t
  -> 'b
  -> 'at
  -> ('a -> Sexp.t)
  -> ('bt -> Sexp.t)
  -> f:('a -> 'b)
  -> unit
