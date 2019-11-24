(** The output signature of the functors [Mk], [Make], [MakeWeak] and [Fake].*)
module type S = sig
  type t

  val memo : ((t -> 'a) -> t -> 'a) -> t -> 'a
end

(** Functions on the initial size used when creating a cache, you can change get
  the current value, update it to a number of your choice, or reset it to the
    default value. *)
let get_initial_cache_size, set_initial_cache_size, reset_initial_cache_size =
  let default = 512 in
  let initial_cache_size = ref default in
  ( (fun () -> !initial_cache_size)
  , (fun size -> initial_cache_size := size)
  , fun () -> initial_cache_size := default )

(** [mk_memo create find add ff] gives a memoïzed version of the functional [ff]
  using the functions [create], [find] and [add] for the cache. It's used
    internally and you shouldn't have to use it. *)
let mk_memo create find add ff =
  let cache = create (get_initial_cache_size ()) in
  let rec f k =
    try find cache k
    with Not_found ->
      let v = ff f k in
      add cache k v ; v
  in
  f

(** With the [Mk] functor, you can also directly provide a [Cache] module, which
  should have the signature [Hashtbl.S]. We will include your cache module and
    use it to define a [memo] function. It should be useful only if you want to
    use another [Hashtbl] implementation or things like this. *)
module Mk (Cache : Hashtbl.S) = struct
  include Cache

  let memo ff = mk_memo Cache.create Cache.find Cache.add ff
end

(** Functor that works like the [Make] one, but the bindings in the memoïzation
  cache will be weak, allowing the garbage collector to remove them if they are
    not used somewhere else. *)
module MakeWeak (H : Hashtbl.HashedType) = Mk (Ephemeron.K1.Make (H))

(** Functor that can be useful in case you don't want to use polymorphic
  equality or you are doing things like hashconsing and you know how to compare
    or hash your type more efficiently. *)
module Make (H : Hashtbl.HashedType) = Mk (Hashtbl.Make (H))

(** Functor that is useful if you want to quickly test a function you memoïzed
  with our [Make] or [MakeWeak] functor, but without memoïzing it. It'll
    basically do nothing and should be equivalent to your initial non-memoïzed
    function. *)
module Fake (H : Hashtbl.HashedType) = Mk (struct
  include Hashtbl.Make (H)

  let find _ _ = raise_notrace Not_found

  let add _ _ _ = ()
end)

(** [memo ff] gives you a memoïzed version of the [ff] functional. *)
let memo ff =
  let open Hashtbl in
  mk_memo create find add ff
