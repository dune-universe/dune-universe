(** Finite maps and sets based on red black trees. *)

open Module_types

module Map (Key: SORTABLE):
sig
    type _ t

    val empty: _ t

    val is_empty: _ t -> bool

    val fold: (Key.t -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

    val cardinal: _ t -> int

    val bindings: 'a t -> (Key.t * 'a) list

    val maybe_find: Key.t -> 'a t -> 'a option

    val find: Key.t -> 'a t -> 'a

    val mem: Key.t -> 'a t -> bool

    val add: Key.t -> 'a -> 'a t -> 'a t

    val remove: Key.t -> 'a t -> 'a t
end





module Set (Element: SORTABLE):
sig
    type t

    val is_empty: t -> bool

    val cardinal: t -> int

    val empty: t

    val mem: Element.t -> t -> bool

    val add: Element.t -> t -> t

    val remove: Element.t -> t -> t
end
