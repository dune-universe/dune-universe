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

(** Automatic Taylor Expansion *)

(** Re-define usual operators to compute values and taylor coefficients
    for elements of type T.t.
    @param T module of operators over the underlying type on which we perform
              automatic differentiation *)
module TTypeName(T : Types.OpS) :
sig
  include Types.OpS with type elt = T.elt
                    and type scalar = T.scalar

  (** {1 Operators} *)

  type op = ..

  type op += CONST | SCALE of scalar | TRANS of scalar
          | SIN of T.t array | COS of T.t array
          | ADD | SUB | MUL | DIV | POW
          | POS | NEG | INV | SQR | SQRT | EXP | LOG | TAN
          | ASIN | ACOS | ATAN

  val string_of_op : op -> string

  (** {1 Additionnal constructors} *)

  val lift : T.t -> t

  (** {1 Accessors} *)

  val value : t -> T.t
  (** Same as {!get} but returns an {!T.t} instead of an {!elt} *)

  val operator : t -> op
  (** [operator f] retrieves the operator represented by [f] *)

  val order : t -> int
  (** Current number of coefficients that have been computed *)

  (** {1  Constructors} *)

  val un_op : op -> t -> t
  (** [un_op op f] builds a node that represents the expression [op(f)] where
      [op] is a unary operator. This is useful to write custom operators *)

  val bin_op : op -> t -> t -> t
  (** [bin_op op f1 f2] builds a node that represents the expression [op(f1,f2)]
      where [op] is a binary operator. This is useful to write custom operators
  *)

  (** {1  Automatic Taylor Expansion} *)

  val set : t -> int -> T.t -> unit
  (** [set f i v] sets the [i]-th coefficient of [f] to [v], this is used to
      set the direction of the expansion. For example, to expand wrt. a variable
      x, one would call [set x 1 1.].
      This can also be used to change the value of a variable: eg. [set x 0 5.].
  *)

  val d : t -> int -> elt
  (** [d f i] retrieves the coefficient of order [i] in
      computation [f] as an [elt]. Must be called after {!eval}. *)

  val deriv : t -> int -> T.t
  (** [deriv f i] retrieves the coefficient of order [i] in
      computation [f]. Must be called after {!eval}. *)

  val get_tvalues : t -> elt array
  (** [get_tvalues f] retrieves the array of taylor coefficients of [f].
      Must be called after {!eval}. *)

  val get_derivatives : t -> elt array
  (** [get_derivatives f] retrieves the array of derivatives of [f].
      Must be called after {!eval}. *)

  val eval : t -> int -> int
  (** [eval f i] propagates the first [i] taylor coefficients of f.
      The direction of the expansion should have been set using {!set}. *)

  val reset : t -> unit
  (** [reset f] resets all the arrays of coefficients. It is used to compute
      a new expansion on the same expression without building a new graph. *)
end

(** Extends {!TTypeName} with comparison operators.
    @param T module of operators over the underlying type on which we perform
              automatic differentiation *)
module OrderedTTypeName(T : Types.OrderedOpS) :
sig
  include Types.OrderedOpS with type elt = T.elt
                           and type scalar = T.scalar

  (** {1 Operators} *)

  type op = ..

  type op += CONST | SCALE of scalar | TRANS of scalar
  | SIN of T.t array | COS of T.t array
  | ADD | SUB | MUL | DIV | POW
  | POS | NEG | INV | SQR | SQRT | EXP | LOG | TAN
          | ASIN | ACOS | ATAN

  val string_of_op : op -> string

  (** {1 Additionnal constructors} *)

  val lift : T.t -> t

  (** {1 Accessors} *)

  val value : t -> T.t
  (** Same as {!get} but returns an {!T.t} instead of an {!elt} *)

  val operator : t -> op
  (** [operator f] retrieves the operator represented by [f] *)

  val order : t -> int
  (** Current number of coefficients that have been computed *)

  (** {1  Constructors} *)

  val un_op : op -> t -> t
  (** [un_op op f] builds a node that represents the expression [op(f)] where
      [op] is a unary operator. This is useful to write custom operators *)

  val bin_op : op -> t -> t -> t
  (** [bin_op op f1 f2] builds a node that represents the expression [op(f1,f2)]
      where [op] is a binary operator. This is useful to write custom operators
  *)

  (** {1  Automatic Taylor Expansion} *)

  val set : t -> int -> T.t -> unit
  (** [set f i v] sets the [i]-th coefficient of [f] to [v], this is used to
      set the direction of the expansion. For example, to expand wrt. a variable
      x, one would call [set x 1 1.].
      This can also be used to change the value of a variable: eg. [set x 0 5.].
  *)

  val d : t -> int -> elt
  (** [d f i] retrieves the coefficient of order [i] in
      computation [f] as an [elt]. Must be called after {!eval}. *)

  val deriv : t -> int -> T.t
  (** [deriv f i] retrieves the coefficient of order [i] in
      computation [f]. Must be called after {!eval}. *)

  val get_tvalues : t -> elt array
  (** [get_tvalues f] retrieves the array of taylor coefficients of [f].
      Must be called after {!eval}. *)

  val get_derivatives : t -> elt array
  (** [get_derivatives f] retrieves the array of derivatives of [f].
      Must be called after {!eval}. *)

  val eval : t -> int -> int
  (** [eval f i] propagates the first [i] taylor coefficients of f.
      The direction of the expansion should have been set using {!set}. *)

  val reset : t -> unit
  (** [reset f] resets all the arrays of coefficients. It is used to compute
      a new expansion on the same expression without building a new graph. *)
end
