
(* This file is free software, part of OLinq. See file "license" for more details. *)

(** {1 Polymorphic Maps and Multimaps} *)

type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a hash = 'a -> int
type 'a iter = ('a -> unit) -> unit

(** {2 Basics} *)

type ('a, +'b) t = private {
  is_empty : unit -> bool;
  size : unit -> int; (* Number of keys *)
  get_exn : 'a -> 'b;
  iter : ('a -> 'b -> unit) -> unit;
  fold : 'c. ('c -> 'a -> 'b -> 'c) -> 'c -> 'c;
  choose: (unit -> ('a * 'b) option);
}
(** Map from keys of type ['a] to values of type ['b]
    {b the type might change},
      it is exposed merely for variance checks w.r.t GADTs. Do not access
      fields directly. *)

type ('a, +'b) map = ('a, 'b) t

val get : ('a,'b) t -> 'a -> 'b option

val get_exn : ('a,'b) t -> 'a -> 'b

val mem : ('a,_) t -> 'a -> bool

val size : (_,_) t -> int

val to_iter : ('a, 'b) t -> ('a * 'b) iter

val to_iter_multimap : ('a, 'b list) t -> ('a * 'b) iter

val to_list : ('a, 'b) t -> ('a * 'b) list

val to_rev_list : ('a, 'b) t -> ('a * 'b) list

val fold : ('acc -> 'a -> 'b -> 'acc) -> 'acc -> ('a,'b) t -> 'acc
(** Fold on the items of the map *)

val fold_multimap : ('acc -> 'a -> 'b -> 'acc) -> 'acc ->
  ('a,'b list) t -> 'acc
(** Fold on the items of the multimap *)

val get_seq : 'a -> ('a, 'b) t -> 'b iter
(** Select a key from a map and wrap into iter *)

val iter : ('a,'b) t -> ('a -> 'b -> unit) -> unit
(** View a multimap as a proper collection *)

val choose : ('a, 'b) t -> ('a * 'b) option

(** {2 Build}

    Used to build new maps *)

module Build : sig
  type ('a, 'b) t

  val get : ('a, 'b) t -> ('a, 'b) map

  val add : ('a, 'b) t -> 'a -> 'b -> unit

  val add_multimap : ('a, 'b list) t -> 'a -> 'b -> unit

  val add_count : ('a, int) t -> 'a -> unit

  val update : ('a, 'b) t -> 'a -> f:('b -> 'b) -> or_:'b -> unit
  (** [update build k ~f ~or_] finds the value [v] associated to [k],
      and maps [k] to [f v]. If [k] is not bound, it becomes bound to [or_] *)

  val of_hash :
    ?eq:('a -> 'a -> bool) -> ?hash:('a -> int) -> ?size:int ->
    unit -> ('a, 'b) t

  val of_cmp :
    ?cmp:('a -> 'a -> int) -> unit -> ('a, 'b) t

  type 'a src =
    | Cmp of 'a ord
    | Hash of 'a equal * 'a hash * int
    | Default

  val of_src : 'a src -> ('a, 'b) t

  val src_of_args : ?cmp:'a ord -> ?eq:'a equal -> ?hash:'a hash -> unit -> 'a src

  val make : ?cmp:'a ord -> ?eq:'a equal -> ?hash:'a hash -> unit -> ('a,'b) t
end

(** {2 Misc} *)

val of_iter : ?src:'a Build.src -> ('a * 'b) iter -> ('a, 'b list) t

val of_list : ?src:'a Build.src -> ('a * 'b) list -> ('a, 'b list) t

val count_seq : ?src:'a Build.src -> 'a iter -> ('a, int) t

val map : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
(** Transform values *)

val reverse : ?src:'b Build.src -> ('a,'b) t -> ('b,'a list) t
(** Reverse relation of the map, as a multimap *)

val reverse_multimap : ?src:'b Build.src -> ('a,'b list) t -> ('b,'a list) t
(** Reverse relation of the multimap *)

val flatten : ('a,'b iter) t -> ('a*'b) iter
(** View a multimap as a collection of individual key/value pairs *)

val flatten_l : ('a,'b list) t -> ('a * 'b) iter
(** View a multimap as a list of individual key/value pairs *)

