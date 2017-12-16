(** This module implements a persistent array data structure as
    described in {{:
    http://www.lri.fr/~filliatr/ftp/publis/puf-wml07.ps}"A Persistent
    Union-Find Data Structure" (Sylvain Concohn and Jean-Chrisophe
    Filliâtre}.

    It is meant for managing quick access while keeping peristency in
    backtracking algorithms such as in unification algorithms using
    union find algorithms.

    In particular, when a persistent array [a_0] has been modified
    several times, yielding arrays [a_1], [a_2], ... , [a_N], when
    [a_i] is accessed using the [get] or [set] functions (with [0 <= i
    < N]) than all [a_j] with [i < j <= N] become unaccessible, hence
    a [Unacessible] exception is raised.
*)


module PersistentArray :
sig

  (** The type of the data structure *)
  type 'a t

  (** This exception is raised in case a persistent array [a_0] has
      been modified several times, yielding arrays [a_1], [a_2], ... ,
      [a_N], and that [a_i] is accessed using the [get] or [set]
      functions (with [0 <= i < N]) when one tries to access [a_j]
      with [i < j <= N]. *)
  exception Unaccessible

    
  exception Store_Not_found
    
  exception Not_found

  (** [init n f] returns a persistent array [a] of length [n] such
      that for all [i]. [a.(i)] is set to [f i]. Note that {ewe
      addressing starts at 1}*)
  val init: int -> (int -> 'a) -> 'a t


  (** [make n d] returns a persistent array [a] of length [n] with [d]
      content for all [i]. Note that {ewe addressing starts at 1}*)
  val make: int -> 'a -> 'a t


  (** [of_list_rev l] returns a persistent array of size the length pf
      [l] and containing its elements {e in the reverse order}*)
  val of_list_rev: 'a list -> 'a t

  (** [get i t] returns the value stored in [t] at address [i]
      {e starting at 1}.*)
  val get: int -> 'a t -> 'a

  (** [set i v t] returns a new persistent array [t'] equal to [t]
      except that [t'.(i)=v].*)
  val set: int -> 'a -> 'a t -> 'a t

  (** [length t] returns the length of [t].*)
  val length: 'a t -> int

  (** [print f t] prints the content of [t] without rerooting it (so
      the same arrays remain accessible. *)
  val print : ('a -> string) -> 'a t -> unit

  (** [print_and_reroot f t] prints the content of [t] and reroots it,
      so any further modifier version of this array becomes
      unaccessible. *)
  val print_and_reroot : ('a -> string) -> 'a t -> unit

  (** [copy t] returns a copy of [t], that is, a fresh array
      containing the same elements as [t]. [t] is unchanged.*)

  val copy : 'a t -> 'a t

end
