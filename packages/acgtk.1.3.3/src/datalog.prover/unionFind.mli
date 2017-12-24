(** Modules with this module type should provide Union-Find algorithms
    and the indexed storage data structure. Note that we take the
    opportunity of implementing from scratch such algorithms to allow
    the [find] function returns not only the index of the
    representative and the values it indexes, but also the storage
    data structure, so that the [find] algorithm can modify it, in
    particular with path compression.
*)

module type S = 
sig
  (** The type of the indexed data structure *)
  type 'a t
  
  (** The type of the values (content) that are indexed. It is either
      an actual value of type ['a] or a link to another indexed
      value. If a content at an index [i] points to [i], it is meant
      that to be a variable.*)
  type 'a content = Link_to of int | Value of 'a

  (** Exception raised when a the union of to indexed value can not
      happen. It should be raised by the [union] function when it
      amounts to make the union between to actual values [Value a] and
      [Value b] and [a != b]. *)
  exception Union_Failure

  (** [create l] returns the corresponding indexed storage data
      structure where each value (or link) is indexed by its position
      in [l] (starting at 1). [d] is a data that may or may not be
      used to fill at init the indexed data structure. *)
  val create : 'a content list -> 'a t

  (** [extract ~start:s i t] returns a list of the [i] first elements
      of [t] starting from position [s] (default is 1, first
      position). It is ensured that the results only contain the
      values of representatives (i.e it follows the [Link_to] links
      until the value of the representative before returning it). *)
  val extract : ?start:int -> int -> 'a t -> 'a content list

  (** [find i h] returns not only the index of the representative and
      the values it indexes, but also the storage data structure, so
      that the [find] algorithm can modify it, in particular with path
      compression. If the returned content is a [Link_to j] then
      [j=i].*)
  val find : int -> 'a t -> ((int * 'a content) * 'a t)
  (* the content returned by [find] should not be a link. Can we
     enforce this using polymorphic variants and/or GADT? *)
    
  (** [union i j h] returns a new indexed storage data structure where
      values indexed by [i] and [j] have been unified (ie one of the
      two is now linked to the index of the representative of the
      other. It fails and raises the {! Union_Failure}
      exception if both [i] and [j] representatives index actual
      values [Value a] and [Value b] and [a != b]. *)
  val union : int -> int -> 'a t -> 'a t

  (** [instantiate i t h] returns a new indexed storage data structure
      where the value indexed by [i] and [t] have been unified. It
      fails and raises the {! Union_Failure} exception if
      [i]'s representative indexes an actual values [Value a] such
      that [a] differs from [t]. *)
  val instantiate : int ->  'a  -> 'a t -> 'a t

  (** [cyclic i h] returns a pair [(b,h')] where [b] is [true] if [h]
      has a cycle (following the [Link_to] links) containing [i] and
      [false] otherwise, and where [h'] contains the same information
      as [h] (possibly differently stored, for instance using path
      compression while checking [h] cyclicity. *)
  val cyclic : int -> 'a t -> (bool * 'a t)

  val copy : 'a t -> 'a t

  val to_string : 'a t  -> string
end

(** Modules with this module type should provide an indexed (by [int]
    indexes) storage data structure for ['a] type values and access
    and update functions.
*)
  
module type Store =
sig
  type 'a t
  exception Store_Not_found
    
  (** [empty i] should return an empty indexed storage data structure
      that will allow indexing {e with values from [1] to [i]}. *)
  (*  val empty : int -> 'a t *)

  (** [make i data] should return an indexed storage data structure
      that will allow indexing {e with value [data] from [1] to
      [i]}. *)
  val make : int -> 'a -> 'a t
    
  val get : int -> 'a t -> 'a
  val set : int -> 'a -> 'a t -> 'a t
  val copy : 'a t -> 'a t
  val length : 'a t -> int
(*  val to_string : 'a t -> ('a -> string) -> string*)
end
  
(** This (functor) module implements a {! UnionFind} data structure. The
    [S] parameter is used to try different implementations of indexed
    data structure, in particular eventually persistent arrays as
    described in {{:
    http://www.lri.fr/~filliatr/ftp/publis/puf-wml07.ps}"A Persistent
    Union-Find Data Structure" (Sylvain Conchon and Jean-Chrisophe
    Filliâtre} *)
module Make(St:Store) : S (*with type 'a t='a ST.t*)

module StoreAsMap:Store
