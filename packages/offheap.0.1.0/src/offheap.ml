(** Copies objects out of the OCaml heap where they are not managed by the GC *)

type 'a t

external copy : 'a -> 'a t = "offheap_copy"

external get : 'a t -> 'a = "offheap_get"

external free : 'a t -> unit = "offheap_free"
