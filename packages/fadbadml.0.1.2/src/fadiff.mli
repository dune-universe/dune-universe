(**************************************************************************)
(*                                                                        *)
(*                                FADBADml                                *)
(*                                                                        *)
(*           OCaml port by François Bidet and Ismail Bennani              *)
(*     Based on FADBAD++, written by Ole Stauning and Claus Bendtsen      *)
(*                                                                        *)
(*                          Copyright 2019-2020                           *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C license.    *)
(*                                                                        *)
(**************************************************************************)

(** Re-define usual operators to compute values and derivatives for elements of
    type T.t in forward mode.
    @param T module of operators over the underlying type on which we perform
              automatic differentiation *)
module FTypeName (T : Types.OpS) :
sig
  include Types.OpS with type elt = T.elt
                    and type scalar = T.scalar

  (** {1 Additionnal constructors} *)

  val lift : T.t -> t

  (** {1 Accessors} *)

  val value : t -> T.t
  (** Same as {!get} but returns an {!T.t} instead of an {!elt} *)

  val dim: t -> int
  (** Size of differentiation vector *)

  (** {1  Automatic Differentiation} *)

  val diff : t -> int -> int -> unit
  (** [diff x i n] assigns [i] as index of variable [x] out of [n] *)

  val d : t -> int -> elt
  (** [d f i] retrieves the derivative of variable of index [i] in
      computation [f] as an [elt] *)

  val set_deriv : t -> int -> T.t -> unit
  (** [set_deriv f i x] sets the derivative of [f] with respect to 
      the variable of index [i] to [x] *)

  val deriv : t -> int -> T.t
  (** [deriv f i] retrieves the derivative of variable of index [i] in
      computation [f] *)

  val reset_diff : t -> unit
end

(** Extends {!FTypeName} with comparison operators.
    @param T module of operators over the underlying type on which we perform
              automatic differentiation *)
module OrderedFTypeName(T : Types.OrderedOpS) :
sig
  include Types.OrderedOpS with type elt = T.elt
                           and type scalar = T.scalar

  (** {1 Additionnal constructors} *)

  val lift : T.t -> t

  (** {1 Accessors} *)

  val value : t -> T.t
  (** Same as {!get} but returns an {!T.t} instead of an {!elt} *)

  val dim: t -> int
  (** Size of differentiation vector *)

  (** {1  Automatic Differentiation} *)

  val diff : t -> int -> int -> unit
  (** [diff x i n] assigns [i] as index of variable [x] out of [n] *)

  val set_deriv : t -> int -> T.t -> unit
  (** [set_deriv f i x] sets the derivative of [f] with respect to 
      the variable of index [i] to [x] *)

  val d : t -> int -> elt
  (** [d f i] retrieves the derivative of variable of index [i] in
      computation [f] as an [elt] *)

  val deriv : t -> int -> T.t
  (** [deriv f i] retrieves the derivative of variable of index [i] in
      computation [f] *)

  val reset_diff : t -> unit
end
