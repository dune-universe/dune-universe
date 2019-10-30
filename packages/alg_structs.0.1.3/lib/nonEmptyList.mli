(** Non-empty lists *)

(** The type of lists which cannot be empty. *)
type 'a t

include Functor.S with type 'a t := 'a t

(** See [List.equal]. *)
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

(** See [List.compare]. *)
val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

(** See [List.cons]. *)
val cons : 'a -> 'a t -> 'a t

(** [uncons t] is [(List.hd t, List.tl t)]. *)
val uncons : 'a t -> ('a * 'a list)

(** [hd t] is the first element of [t].

    Since [t] is non-empty, it is guaranteed that there will always be a head. *)
val hd : 'a t -> 'a

(** [tl t] are the elements of [t] after, but not including the head.

    Since [t] is non-empty, it is guaranteed that there will always be a tail. *)
val tl : 'a t -> 'a list

(** See [List.append]. *)
val append : 'a t -> 'a t -> 'a t

(** An alias for {!val:append}. *)
val op : 'a t -> 'a t -> 'a t

(** See [List.concat]. *)
val fold : ('a -> 'a -> 'a) -> 'a t -> 'a

(** [of_list xs] is a [Some] non-empty list of [xs] if [xs] isn't empty.
    Otherwise it is [None]. *)
val of_list : 'a list -> 'a t option

(** [to_list t] is the list of the elements in [t]. *)
val to_list : 'a t -> 'a list
