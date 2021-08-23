(** Module types useful for metaprogramming. *)

(** Abstraction for standard boilerplate *)
module type Std = sig
  type 'a m

  type t

  val compare : t m -> t m -> int m

  val equal : t m -> t m -> bool m

  val pp : Format.formatter m -> t m -> unit m

  val hash : t m -> int m
end

(** Empty abstraction. *)
module type Empty = sig
  (** ['a m] is the type of programs computing a value of type ['a] *)
  type 'a m
end

(** Abstraction for sequencing. Expected to be associative. *)
module type Sequencing = sig
  type 'a m

  (** Non-binding sequencing construct. *)
  val seq : unit m -> (unit -> 'a m) -> 'a m

  (** Binding sequencing construct. *)
  val ( let* ) : 'a m -> ('a m -> 'b m) -> 'b m

  val unit : unit m
end

(** Abstraction for loops. *)
module type Loop = sig
  type 'a m

  (** The type of loop indices. *)
  type index

  (** [for_ ~start ~stop f] is a for loop, iterating [f] in
      the interval [[start;stop]] in order. *)
  val for_ : start:index m -> stop:index m -> (index m -> unit m) -> unit m

  (** While loop. *)
  val while_ : cond:(unit -> bool m) -> (unit -> unit m) -> unit m
end

(** Abstraction for constants. *)
module type Const = sig
  type 'a m

  (** The type of cosntants. *)
  type t

  (** Injects a constant from the meta to the object language. *)
  val const : t -> t m
end

(** Abstraction for abstraction :) *)
module type Lambda = sig
  type 'a m

  (** Lambda-abstraction. *)
  val lam : ('a m -> 'b m) -> ('a -> 'b) m

  (** Function application. *)
  val app : ('a -> 'b) m -> 'a m -> 'b m
end

(** Abstraction for products. *)
module type Product = sig
  type 'a m

  (** Product-forming. *)
  val prod : 'a m -> 'b m -> ('a * 'b) m

  (** First projection. *)
  val fst : ('a * 'b) m -> 'a m

  (** Last projection. *)
  val snd : ('a * 'b) m -> 'b m
end

(** Abstraction for monomorphic storage. *)
module type Storage = sig
  type 'a m

  (** The type of mutable storage. *)
  type t

  (** The type of data stored in the storage. *)
  type elt

  (** [create v] creates a mutable storage with initial content [v]. *)
  val create : elt m -> t m

  (** Update a mutable storage. *)
  val set : t m -> elt m -> unit m

  (** Get the current value in a mutable storage. *)
  val get : t m -> elt m
end

(** Abstraction for booleans. *)
module type Bool = sig
  type 'a m

  val true_ : bool m

  val false_ : bool m

  val ( || ) : bool m -> bool m -> bool m

  val ( && ) : bool m -> bool m -> bool m

  (** [dispatch cond branches] constructs an if-then-else branching
      expression. *)
  val dispatch : bool m -> (bool -> 'a m) -> 'a m
end

(** Abstraction for enums. *)
module type Enum = sig
  type 'a m

  (** The type of enumerations. *)
  type t

  (** All cases in the enumeration.*)
  val all : t array

  (** [enum x] is the index of [x] in [all]. *)
  val enum : t -> int

  (** Injects a case of the enumeration from the meta to the object language. *)
  val const : t -> t m

  (** Dispatch on a value of the enumeration. *)
  val dispatch : t m -> (t -> 'a m) -> 'a m
end

(** Abstraction for arrays. *)
module type Array = sig
  type 'a m

  type t

  type elt

  type index

  val get : t m -> index m -> elt m

  val set : t m -> index m -> elt m -> unit m

  val make : index m -> elt m -> t m

  val length : t m -> index m

  val unsafe_get : t m -> index m -> elt m

  val unsafe_set : t m -> index m -> elt m -> unit m

  val copy : t m -> t m

  val blit : t m -> index m -> t m -> index m -> index m -> unit m

  val sub : t m -> index m -> index m -> t m
end

(** Abstraction for rings. *)
module type Ring = sig
  type 'a m

  type t

  include Intf_algebra.Ring with type t := t m
end

(** Ring equipped with standard boilerplate. *)
module type Ring_std = sig
  include Ring

  include Std with type t := t and type 'a m := 'a m
end

(** Abstraction for Fields. *)
module type Field = sig
  type 'a m

  type t

  include Intf_algebra.Field with type t := t m
end

(** Field equipped with standard boilerplate. *)
module type Field_std = sig
  include Field

  include Std with type t := t and type 'a m := 'a m
end

(** Abstraction for raising exceptions. *)
module type Exn = sig
  type 'a m

  val raise_ : exn -> 'a m
end

(** Infix ordering operators. *)
module type Infix_order = Intf_infix.Infix_order

(* Doing a bit on introspection on where I got these "shapes" from, these probably
   come from the work on containers by the Scottish PL gang (McBride, Ghani et al)
   as well as from readings on the theory of combinatorial species (two related
   pieces of work). *)

(** Shapes: folding and iterating over abstract indexed structures. *)
module type Shape = sig
  type 'a m

  (** The type of (one-dimensional) positions in a shape. *)
  type pos

  (** The type of shapes, parametric in the type of positions. *)
  type 'a t

  (** [mem p s] is true iff [p] is a position in the shape [s]. *)
  val mem : 'a t -> 'a m -> bool m

  (** [pos_equal s p p'] is true iff [p] and [p'] are equal positions
      in the shape [s]. *)
  val pos_equal : 'a t -> 'a m -> 'a m -> bool m

  (** [equal s s'] is true iff [s] and [s'] describe equal shapes. *)
  val equal : 'a t -> 'a t -> bool m

  (** First-class mutable storage. *)
  module type Storage = Storage with type 'a m = 'a m
  (* We declare this here to avoid resorting to higher-kinded encodings. *)

  type 'elt storage = (module Storage with type elt = 'elt)

  (** Iterate on a shape. *)
  val iter : ('a m -> unit m) -> 'a t -> unit m

  (** Fold on a shape. *)
  val fold :
    'acc storage -> ('a m -> 'acc m -> 'acc m) -> 'a t -> 'acc m -> 'acc m

  (** Shape morphisms, described as a category. *)
  module Morphism : sig
    type 'a obj := 'a t

    (** The type of morphisms from a tensor indexed by ['a] to a tensor indexed by ['b]. *)
    type ('a, 'b) t

    (** Get the map on positions underlying the shape morphism. *)
    val underlying : ('a, 'b) t -> 'a m -> 'b m

    (** [domain m] is the domain of the morphism [m], ie a tensor indexed by ['a]. *)
    val domain : ('a, 'b) t -> 'a obj

    (** [range m] is the range of the morphism [m], ie a tensor indexed by ['b]. *)
    val range : ('a, 'b) t -> 'b obj

    (** [identity s] is the identity morphism at the shape [s]. *)
    val identity : 'a obj -> ('a, 'a) t

    (** [compose] is sequential morphism composition. *)
    val compose : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  end
end
