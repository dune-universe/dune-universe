(** Modules with this module type should provide Union-Find algorithms
    and the indexed storage data structure. Note that we take the
    opportunity of implementing from scratch such algorithms to allow
    the [find] function returns not only the index of the
    representative and the values it indexes, but also the storage
    data structure, so that the [find] algorithm can modify it, in
    particular with path compression.
*)


(** Modules with this module type should provide an indexed (by [int]
    indexes) storage data structure for ['a] type values and access
    and update functions.
*)
  
 
(** This module implements a {! UnionFind} data structure. The [S]
    parameter is used to try different implementations of indexed data
    structure, in particular eventually persistent arrays as described
    in {{: http://www.lri.fr/~filliatr/ftp/publis/puf-wml07.ps}"A
    Persistent Union-Find Data Structure" (Sylvain Conchon and
    Jean-Chrisophe Filliâtre} *)
module UF(Value :
  sig
    type t
    type value
    val unfold : value -> t -> (int*value list) option
(*    val fold : (int*value list) -> t -> value *)
  end) :
sig
  (** The type of the indexed data structure *)
  type t
  
  (** The type of the values (content) that are indexed. It is either
      an actual value of type ['a] or a link to another indexed
      value. If a content at an index [i] points to [i], it is meant
      that to be a variable.*)
  type content = Link_to of int | Value of Value.value | Constr of (int*int list)

  (** Exception raised when a the union of to indexed value can not
      happen. It should be raised by the [union] function when it
      amounts to make the union between to actual values [Value a] and
      [Value b] and [a != b]. *)
  exception Union_Failure

  (** [create l] returns the corresponding indexed storage data
      structure where each value (or link) is indexed by its position in [l]
      (starting at 1 *)
(*  val create : content list -> Value.t -> t *)


  val empty : t

  val generate_new_var : t -> int * t

  (** [generate_new_constr h (c,var_args)] returns a pair [(j,h')]
      where [h'] is [h] updated with a new index [j] that contains a
      [Constr(c,var_args)] value. [var_args] is a list of already
      defined indexed of [h] (not checked here). *)
  val generate_new_constr : t -> (int*int list) -> int * t

  (** [extract ~start:s i t] returns a list of the [i] first elements
      of [t] starting from position [s] (default is 1, first
      position). It is ensured that the results only contain the
      values of representatives (i.e it follows the [Link_to] links
      until the value of the representative before returning it). *)
  val extract : ?start:int -> int -> t -> content list

  (** [find i h] returns not only the index of the representative and
      the values it indexes, but also the storage data structure, so
      that the [find] algorithm can modify it, in particular with path
      compression. If the returned content is a [Link_to j] then
      [j=i].*)
  val find : int -> t -> ((int * content) * t)
  (* the content returned by [find] should not be a link. Can we
     enforce this using polymorphic variants and/or GADT? *)
    
  (** [union i j h] returns a new indexed storage data structure where
      values indexed by [i] and [j] have been unified (ie one of the
      two is now linked to the index of the representative of the
      other. It fails and raises the {! Union_Failure}
      exception if both [i] and [j] representatives index actual
      values [Value a] and [Value b] and [a != b]. *)
  val union : int -> int -> t -> t

  (** [instantiate i t h] returns a new indexed storage data structure
      where the value indexed by [i] and [t] have been unified. It
      fails and raises the {! Union_Failure} exception if
      [i]'s representative indexes an actual values [Value a] such
      that [a] differs from [t]. *)
  val instantiate : int -> Value.value -> Value.t -> t -> t

  (** [cyclic i h] returns a pair [(b,h')] where [b] is [true] if [h]
      has a cycle (following the [Link_to] links) containing [i] and
      [false] otherwise, and where [h'] contains the same information
      as [h] (possibly differently stored, for instance using path
      compression while checking [h] cyclicity. *)
  val cyclic : int -> t -> (bool * t)

  val copy : t -> t

  val to_string :  t  -> string
end

