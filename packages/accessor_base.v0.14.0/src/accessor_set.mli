open! Base
open! Import

(** Access the existence of a key in a set. [true] means the key is present, and [false]
    means it is absent. *)
val at : 'key -> (_, bool, ('key, 'cmp) Set.t, [< field ]) Accessor.Simple.t

(** The indexed version of [at] adds the given key to the index. *)
val ati
  :  'key
  -> ( 'key * 'i -> bool -> bool
     , 'i -> ('key, 'cmp) Set.t -> ('key, 'cmp) Set.t
     , [< field ] )
       Accessor.t

(** Access [()] iff the set contains the given key. *)
val found : 'key -> (_, unit, ('key, 'cmp) Set.t, [< optional ]) Accessor.Simple.t

(** The indexed version of [found] adds the given key to the index. *)
val foundi
  :  'key
  -> ( 'key * 'i -> unit -> unit
     , 'i -> ('key, 'cmp) Set.t -> ('key, 'cmp) Set.t
     , [< optional ] )
       Accessor.t

(** Access every element in a set. *)
val each : ('i -> 'key -> _, 'i -> ('key, 'cmp) Set.t -> _, [< many_getter ]) Accessor.t

(** Treat [None] equivalently with the empty set. This accessor is not well-behaved, as it
    violates [construct (get at) = at]:

    [construct (get (Some Foo.Set.empty)) = construct Foo.Set.empty = None] *)
val empty_default
  :  ('k1, 'cmp1) Set.comparator
  -> ( 'i -> ('k1, 'cmp1) Set.t -> ('k2, 'cmp2) Set.t
     , 'i -> ('k1, 'cmp1) Set.t option -> ('k2, 'cmp2) Set.t option
     , [< isomorphism ] )
       Accessor.t

(** [of_accessor (module M) accessor x] is a [M.Set.t] that contains everything accessed
    by [accessor] in [x]. *)
val of_accessor
  :  ('a, 'cmp) Set.comparator
  -> (unit -> 'a -> _, unit -> 'at -> _, [> many_getter ]) Accessor.t
  -> 'at
  -> ('a, 'cmp) Set.t
