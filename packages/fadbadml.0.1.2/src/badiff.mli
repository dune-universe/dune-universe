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

(** Backward Automatic Differentiation (BAD) *)

(** Re-define usual operators to compute values and derivatives for elements of
    type T.t in backward mode.
    @param T module of operators over the underlying type on which we perform
              automatic differentiation *)
module BTypeName (T : Types.OpS) :
sig
  include Types.OpS with type elt = T.elt
                    and type scalar = T.scalar

  (** {1 Operators} *)

  type op = ..

  type op +=
    | CONST | SCALE of scalar | TRANS of scalar
    | ADD | SUB | MUL | DIV | POW
    | POS | NEG | INV | SQR | SQRT | EXP | LOG | SIN | COS | TAN
    | ASIN | ACOS | ATAN

  (** {1 Additionnal constructors} *)

  val lift : T.t -> t

  (** {1 Accessors} *)

  val value : t -> T.t
  (** Same as {!get} but returns an {!T.t} instead of an {!elt} *)

  (** {1  Automatic Differentiation} *)

  val diff : t -> int -> int -> unit
  (** [diff x i n] assigns [i] as index of variable [x] out of [n] *)

  val d : t -> int -> elt
  (** [d f i] retrieves the derivative of variable of index [i] in
      computation [f] as an [elt]. Must be called after {!diff} and {!compute}.
  *)

  val deriv : t -> int -> T.t
  (** Same as {!d} but returns an {!T.t} instead of an {!elt} *)

  val compute : t -> unit
  (** [compute f] updates the reference counter of the nodes in the sub-graph
      starting from f so that they are propagated at the right moment. Must
      be called before {!d} or {!deriv}. *)

  val compute_list : t list -> unit
  (** Same as running {!compute} on each element of the input list. *)

  (** {1 Printers} *)
  
  val fprint : Format.formatter -> t -> unit
end

(** Extends {!BTypeName} with comparison operators.
    @param T module of operators over the underlying type on which we perform
              automatic differentiation *)
module OrderedBTypeName (T : Types.OrderedOpS) :
sig
  include Types.OrderedOpS with type elt = T.elt
                           and type scalar = T.scalar

  (** {1 Operators} *)

  type op = ..

  type op +=
    | CONST | SCALE of scalar | TRANS of scalar
    | ADD | SUB | MUL | DIV | POW
    | POS | NEG | INV | SQR | SQRT | EXP | LOG | SIN | COS | TAN
    | ASIN | ACOS | ATAN

  (** {1 Additionnal constructors} *)

  val lift : T.t -> t

  (** {1 Accessors} *)

  val value : t -> T.t
  (** Same as {!get} but returns an {!T.t} instead of an {!elt} *)

  (** {1  Automatic Differentiation} *)

  val diff : t -> int -> int -> unit
  (** [diff x i n] assigns [i] as index of variable [x] out of [n] *)

  val d : t -> int -> elt
  (** [d f i] retrieves the derivative of variable of index [i] in
      computation [f] as an [elt]. Must be called after {!diff} and {!compute}.
  *)

  val deriv : t -> int -> T.t
  (** Same as {!d} but returns an {!T.t} instead of an {!elt} *)

  val compute : t -> unit
  (** [compute f] updates the reference counter of the nodes in the sub-graph
      starting from f so that they are propagated at the right moment. Must
      be called before {!d} or {!deriv}. *)

  val compute_list : t list -> unit
  (** Same as running {!compute} on each element of the input list. *)
end
