(** Copies objects out of the OCaml heap where they are not managed by the GC *)

type 'a t

type alloc



external copy_with_alloc : alloc -> 'a -> 'a t = "offheap_copy_with_alloc"

external get_alloc : unit -> alloc = "offheap_get_alloc"

external get : 'a t -> 'a = "offheap_get"

external delete : 'a t -> unit = "offheap_delete"


let malloc = get_alloc ()

let copy ?(alloc=malloc) obj = copy_with_alloc alloc obj
