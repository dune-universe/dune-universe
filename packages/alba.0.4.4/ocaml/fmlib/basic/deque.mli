type _ t

val is_empty: _ t -> bool

val has_some: _ t -> bool

val empty: _ t

val push_front: 'a -> 'a t -> 'a t

val push_rear: 'a -> 'a t -> 'a t

val pop_front: 'a t -> ('a * 'a t) option

val update_first: ('a -> 'a) -> 'a t -> 'a t

val update_last: ('a -> 'a) -> 'a t -> 'a t

val to_list: 'a t -> 'a list
