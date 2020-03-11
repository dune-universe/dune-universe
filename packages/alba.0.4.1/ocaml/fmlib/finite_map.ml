open Module_types

module type S =
  sig
    type key
    type +'a t
    val empty: 'a t
    val is_empty: 'a t -> bool
    val mem: key -> 'a t -> bool
    val maybe_find: key -> 'a t -> 'a option
    val find: key -> 'a t -> 'a
    val add: key -> 'a -> 'a t -> 'a t
    val remove: key -> 'a t -> 'a t
    val fold:   (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  end

module Make (E:SORTABLE): S with type key = E.t  =
  struct
    include Map.Make (E)
    let maybe_find (k:key) (m:'a t): 'a option =
      try
        Some (find k m)
      with Not_found ->
        None
    let find (k:key) (m:'a t): 'a =
      try
        find k m
      with Not_found ->
        assert false
  end
