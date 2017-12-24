(**************************************************************************)
(*                                                                        *)
(*                 ACG development toolkit                                *)
(*                                                                        *)
(*                  Copyright 2008 INRIA                                  *)
(*                                                                        *)
(*  More information on "http://acg.gforge.inria.fr/"                     *)
(*  License: CeCILL, see the LICENSE file or "http://www.cecill.info"     *)
(*  Authors: see the AUTHORS file                                         *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*                                                                        *)
(*  $Rev::                              $:  Revision of last commit       *)
(*  $Author::                           $:  Author of last commit         *)
(*  $Date::                             $:  Date of last commit           *)
(*                                                                        *)
(**************************************************************************)

(** This module implements a functor returning an {e non-funtional}
    lookup table when provided with a given size *)


(** The signature module for modules providing size *)
module type BASE =
sig
  (** The size value *)
  val b : int
end

(** The signature module for tables *)
module type TABLE =
sig
  (** Exceptions raised by functions *)
  exception Not_found
  exception Conflict

  (** The type of the tables *)
  type 'a t

  (** The type of the key *)
  type key
  
  (** [empty] returns the empty table *)
  val empty : 'a t

  (** [add k v t] modifies the table [t] to add the element [v] at key
      [k]. The optional value [override] is set to false by
      default. When set to false, the [add] function raises
      {!Table.TABLE.Conflict} when the key [k] was already associated
      with some value. When set to [true], the [add] function does not
      raise [!Table.TABLE.Conflict] if some value was already
      associated to the key *)
  val add : ?override:bool -> key -> 'a -> 'a t -> 'a t

  (** [find k t] returns the element associated with the key [k] in
      [t]. Raises {!Table.TABLE.Not_found} if no such element
      exists *)
  val find : key -> 'a t -> 'a
    
  (** [fold f a t] returns [f kn vn (f kn-1 vn-1 (...(f k1 v1 a)
      ...))] where the [ki] and [vi] are the associated values in
      [t]. The elements are listed in order wrt. to the key *)
  val fold : (key -> 'a -> 'b -> 'b) -> 'b -> 'a t -> 'b
end

(** This modules provides the functor *)
module Make_table (Base : BASE) : TABLE with type key=int
