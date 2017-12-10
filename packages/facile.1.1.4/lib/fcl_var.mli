(***********************************************************************)
(*                                                                     *)
(*                           FaCiLe                                    *)
(*                 A Functional Constraint Library                     *)
(*                                                                     *)
(*            Nicolas Barnier, Pascal Brisset, LOG, CENA               *)
(*                                                                     *)
(* Copyright 2004 CENA. All rights reserved. This file is distributed  *)
(* under the terms of the GNU Lesser General Public License.           *)
(***********************************************************************)
(* $Id: fcl_var.mli,v 1.31 2004/09/03 13:25:55 barnier Exp $ *)

(** {1 Constrained, Attributed, Finite Domain Variables} *)


module type ATTR = sig
    type t
(** Type of attributes. *)

    type domain
(** Type of domains stored in attributes. *)

    type elt
(** Type of element of domains. *)
    
    type event
(** Type of events (modifications on variables) on which to suspend. *)
    
    val dom : t -> domain
(** [dom a] returns the integer domain of an attribute. *)
    
    val on_refine : event
(** Event occuring when a variable is changed, i.e. its domain modified. *)
    
    val on_subst : event
(** Event occuring when a variable is instantiated. *)
    
    val on_min : event
    val on_max : event
(** Event occuring when the lower (resp. upper) bound of a variable decreases.*)
    
    val fprint : out_channel -> t -> unit
(** [fprint chan a] prints attribute [a] on channel [chan]. *)
    
    val min : t -> elt
    val max : t -> elt
(** [min a] (resp. [max a]) returns the lower (resp. upper) bound of [a]. *)
    
    val member : t -> elt -> bool
(** [member a n] tests if [n] belongs to [dom a]. *)
    
    val id : t -> int
(** [id a] returns a unique integer identifying the attribute [a]. *)
    
    val constraints_number : t -> int
(** [constraints_number a] returns the number of different constraints
   attached to [a]. *)
    
    val size : t -> int
(** [size a] returns the number of integer values in the domain of [a]. *)
  end
(** Signature of the Attribute of a Domain Variable. A module
   endowed with this type is required to build finite domain variables. 
   [Domain] and [SetDomain] are suitable domain modules. *)


module Attr : ATTR
  with type domain = Fcl_domain.t and type elt = Fcl_domain.elt
module SetAttr : ATTR
  with type domain = Fcl_setDomain.t and type elt = Fcl_setDomain.S.t

type ('a, 'b) concrete = Unk of 'a | Val of 'b
(** Concrete type of the value of finite domain variables. *)

module type BASICFD = sig
  type t
(** Type of finite domain variable. *)
  
  type attr
(** Type of attributes. *)
  
  type domain
(** Type of domains. *)
  
  type elt
(** Type of elements of domains. *)
  
  type event
(** Type of domain reduction events. *)

(** {2 Creation} *)

  val create : ?name:string -> domain -> t
(** [create ?name d] returns a new variable with domain [d]. If provided,
    [name] will be used by the pretty printer. *)
  
  val interval : ?name:string -> elt -> elt -> t
(** [interval ?name inf sup] returns a new variable with domain [[inf..sup]].
    If provided, [name] will be used by the pretty printer.*)
  
  val array : ?name:string -> int -> elt -> elt -> t array
(** [array n inf sup] returns an array of [n] new variables with domain
   [[inf..sup]]. If provided, [name] (suffixed with the index of the element)
   will be used by the pretty printer. *)
  
  val elt : elt -> t
(** [int n] returns a new variable instantiated to integer value [n]. *)

(** {2 Access} *)

  val is_var : t -> bool
(** [is_var v] returns [true] if [v] is not instantiated and [false]
   otherwise. *)
  
  val is_bound : t -> bool
(** [is_bound v] returns [true] if [v] is instantiated and [false]
   otherwise. *)
  
  val value : t -> (attr, elt) concrete
(** [value v] returns [Val n] if [v] is instantiated to [n], [Unk a] otherwise
   where [a] is the attribute of [v]. Should always be used in a matching:
   [match value v with Val n -> ... | Unk a -> ...]. *)
  
  val min : t -> elt
(** [min v] returns the lower bound of [v]. *)
  
  val max : t -> elt
(** [max v] returns the upper bound of [v]. *)
  
  val min_max : t -> elt * elt
(** [min_max v] returns both the lower and upper bounds of [v]. *)
  
  val elt_value : t -> elt
(** [int_value v] returns the value of [v] if it is instantiated and raises
   a [Failure] exception otherwise. *)
  
  val int_value : t -> elt
(*
  [int_value = elt_value] deprecated.
*)
  
  val size : t -> int
(** [size v] returns the number of integer values in the domain of [v]
   ([1] if [v] is instantiated). *)
  
  val member : t -> elt -> bool
(** [member v n] returns [true] if [n] belongs to the domain of [v] and
   [false] otherwise. *)
  
  val id : t -> int
(** [id v] returns a unique integer identifying the attribute associated
   with [v]. Must be called only on non ground variable, raise [Failure]
   otherwise. *)
  
  val name : t -> string
(** [name v] returns the name of variable [v] (the empty string if
   it was not provided while created). Must be called only on non ground
   variable, raise [Failure] otherwise. *)
  
  val compare : t -> t -> int
  (** Compares two variables. Values (bound variables) are smaller than
     unknowns (unbound variables). Unknowns are sorted according to
     their attribute [id]. *)
  
  val equal : t -> t -> bool
  (** Tests if two variables are equal with respect to [compare]. *)
  
  val fprint : out_channel -> t -> unit
(** [fprint chan v] prints variable [v] on channel [chan]. *)
  
  val fprint_array : out_channel -> t array -> unit
(** [fprint_array chan vs] prints array of variables [vs] on channel [chan]. *)

(** {2 Refinement} *)

  val unify : t -> elt -> unit
(** [unify v n] instantiates variable [v] with integer value [n]. Raises
   [Fcl_stak.Fail] in case of failure. [unify] may be called either on unbound
   variables or on instantiated variables. *)
  
  val refine : t -> domain -> unit
(** [refine v d] reduces the domain of [v] with domain [d]. [d] must be
   included in the domain of [v], otherwise the behaviour is
   unspecified (corrupted system or exception raised). *)
  
  val refine_low : t -> elt -> unit
(** [refine_low v inf] reduces the domain of [v] by cutting all values
    strictly less than [inf]. *)
  
  val refine_up : t -> elt -> unit
(** [refine_up v sup] reduces the domain of [v] by cutting all values
    strictly greater than [sup]. *)
  
  val refine_low_up : t -> elt -> elt -> unit
(** [refine_low_up v inf sup] reduces the domain of [v] by cutting all values
    strictly less than [inf] and greater than [sup]. Robust even if [v]
    is already bound (checks that [inf] <= [v] <= [sup], otherwise fails). *)

(** {2 Events and suspending} *)

  val on_refine : event
(** Event occuring when a variable is changed, i.e. its domain modified. *)
  
  val on_subst : event
(** Event occuring when a variable is instantiated. *)
  
  val on_min : event
  val on_max : event
(** Event occuring when the lower (resp. upper) bound of a variable decreases. *)
  
  val delay : event list -> t -> ?waking_id:int -> Fcl_cstr.t -> unit
(** [delay event_list v ~waking_id:id c] suspends constraint [c] on all
   the events in [event_list] occurring on [v]. An optional integer
   [id] may be associated to the wakening: it must be unique and range
   from 0 to [nb_wakings-1], [nb_wakings] being the argument of [Cstr.create]
   specifying the number of calls to [delay] with distinct [waking_id]
   arguments. These integers are arguments to the "update" function of
   constraints and aim at discriminating waking events to fire the
   appropriate propagation rule. [waking_id] default value is 0.
   This function has no effect on instantiated variables (as no event
   could occur on a ground variable). *)

(**/**)

  val int : elt -> t
  val subst : t -> elt -> unit
(** [subst v n] instantiates variable [v] with integer value [n]. Raises
   [Fcl_stak.Fail] in case of failure. Must be called only on unbound
   (not instantiated) variable, otherwise a [Failure] exception is raised. *)
  
  val unify_cstr : t -> elt -> Fcl_cstr.t
end
(** Common variables module signature. *)

(** Extended signature for finite domain variable (with added functions
   irrelevant to set variables). *)
module type FD = sig
  include BASICFD
  val remove : t -> elt -> unit
(** [remove v a] removes [a] from the domain of [v]. Leaves the domain
   unchanged if [a] does not belong to it. *)
  
  val values : t -> elt list
(** [values v] returns the list of all integers in the domain of [v]. If
   [v] is instantiated to [n], returns the singleton list containing [n]. *)
  
  val iter : (elt -> unit) -> t -> unit
(** [iter f v] iterates f on each integer in the domain of [v]. *)
end

module Fd : FD
 with
     type domain = Fcl_domain.t
     and type elt = Fcl_domain.elt
     and type attr = Attr.t
     and type event = Attr.event
(** Concrete finite domain variable module. *)

module SetFd : BASICFD
 with
     type domain = Fcl_setDomain.t
     and type elt = Fcl_setDomain.S.t
     and type attr = SetAttr.t
     and type event = SetAttr.event
(** Concrete integer set variable module. *)

(**/**)
(** Obsolete, for backward compatibility only *)
type concrete_fd = (Fd.attr, Fd.elt) concrete
val delay : Attr.event list -> Fd.t -> ?waking_id:int -> Fcl_cstr.t -> unit
