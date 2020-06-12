open! Base
open! Import

(** See also: [Accessor_core.Map] *)

(** If [key] maps to some [data] in the map being accessed, [at key] accesses [Some data],
    otherwise it accesses [None]. You can use [at key] to determine or control whether
    there is a mapping for [key] at all. For example, [Accessor.set (at key) map None]
    removes any mapping for [key] in [map]. *)
val at
  :  'key
  -> (_, 'data option, ('key, 'data, 'cmp) Map.t, [< field ]) Accessor.Simple.t

(** [ati] is the indexed version of [at]. The index is simply whatever key was supplied as
    an argument. *)
val ati
  :  'key
  -> ( 'key * 'i -> 'data option -> 'data option
     , 'i -> ('key, 'data, 'cmp) Map.t -> ('key, 'data, 'cmp) Map.t
     , [< field ] )
       Accessor.t

(** [found key] is like [at key], but it does not give any control over whether [key]
    exists in the map. It either accesses the data or it doesn't, depending on whether it
    exists. *)
val found
  :  'key
  -> (_, 'data, ('key, 'data, 'cmp) Map.t, [< optional ]) Accessor.Simple.t

(** [foundi] is the indexed version of [found]. The index is simply whatever key was
    supplied as an argument. *)
val foundi
  :  'key
  -> ( 'key * 'i -> 'data -> 'data
     , 'i -> ('key, 'data, 'cmp) Map.t -> ('key, 'data, 'cmp) Map.t
     , [< optional ] )
       Accessor.t

(** [each] accesses each datum in the map. *)
val each
  : ( 'i -> 'a -> 'b
    , 'i -> ('k, 'a, 'cmp) Map.t -> ('k, 'b, 'cmp) Map.t
    , [< many ] )
      Accessor.t

(** [eachi] is the indexed version of [each]. The indices are the keys that map to the
    data being accessed. *)
val eachi
  : ( 'k * 'i -> 'a -> 'b
    , 'i -> ('k, 'a, 'cmp) Map.t -> ('k, 'b, 'cmp) Map.t
    , [< many ] )
      Accessor.t

(** [empty_default (module M)] is an isomorphism between [Map.t option] and [Map.t],
    treating [None] identically with [Map.empty (module M)]. Note that this isn't
    well-behaved in general because it doesn't satisfy [construct (get a) = a]:

    {[
      construct (get (Some (Map.empty (module M))))
      = construct (Map.empty (module M))
      = None
    ]} *)
val empty_default
  :  ('k1, 'cmp1) Map.comparator
  -> ( 'i -> ('k1, 'a, 'cmp1) Map.t -> ('k2, 'b, 'cmp2) Map.t
     , 'i -> ('k1, 'a, 'cmp1) Map.t option -> ('k2, 'b, 'cmp2) Map.t option
     , [< isomorphism ] )
       Accessor.t

(** [of_accessor (module M) accessor x ~key_of_index] is a [M.Map.t] created by traversing
    [x] with [accessor], mapping each index to the data being accessed. *)
val of_accessor
  :  ('k, 'cmp) Map.comparator
  -> ('i -> 'a -> _, unit -> 'at -> _, [> many_getter ]) Accessor.t
  -> 'at
  -> key_of_index:('i Accessor.Index.t -> 'k)
  -> [ `Duplicate_key of 'k | `Ok of ('k, 'a, 'cmp) Map.t ]

(** Raising version of [of_accessor]. *)
val of_accessor_exn
  :  ('k, 'cmp) Map.comparator
  -> ('i -> 'a -> _, unit -> 'at -> _, [> many_getter ]) Accessor.t
  -> 'at
  -> key_of_index:('i Accessor.Index.t -> 'k)
  -> ('k, 'a, 'cmp) Map.t

(** [Or_error] version of [of_accessor]. *)
val of_accessor_or_error
  :  ('k, 'cmp) Map.comparator
  -> ('i -> 'a -> _, unit -> 'at -> _, [> many_getter ]) Accessor.t
  -> 'at
  -> key_of_index:('i Accessor.Index.t -> 'k)
  -> ('k, 'a, 'cmp) Map.t Or_error.t

(** A version of [of_accessor] that allows you to accumulate multiple values per key with
    a function. *)
val of_accessor_fold
  :  ('k, 'cmp) Map.comparator
  -> ('i -> 'a -> _, unit -> 'at -> _, [> many_getter ]) Accessor.t
  -> 'at
  -> key_of_index:('i Accessor.Index.t -> 'k)
  -> init:'acc
  -> f:('acc -> 'a -> 'acc)
  -> ('k, 'acc, 'cmp) Map.t

(** A version of [of_accessor] that allows you to collect multiple values per key into a
    list. *)
val of_accessor_multi
  :  ('k, 'cmp) Map.comparator
  -> ('i -> 'a -> _, unit -> 'at -> _, [> many_getter ]) Accessor.t
  -> 'at
  -> key_of_index:('i Accessor.Index.t -> 'k)
  -> ('k, 'a list, 'cmp) Map.t

(** A version of [of_accessor] that allows you to combine multiple values per key with a
    function. *)
val of_accessor_reduce
  :  ('k, 'cmp) Map.comparator
  -> ('i -> 'a -> _, unit -> 'at -> _, [> many_getter ]) Accessor.t
  -> 'at
  -> key_of_index:('i Accessor.Index.t -> 'k)
  -> f:('a -> 'a -> 'a)
  -> ('k, 'a, 'cmp) Map.t

include
  Accessor.Applicative_without_return.S3
  with type ('data, 'key, 'cmp) t := ('key, 'data, 'cmp) Map.t
