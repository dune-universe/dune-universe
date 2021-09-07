(* Copyright (C) 2021 Alan Hu <alanh@ccs.neu.edu>

   This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at https://mozilla.org/MPL/2.0/. *)

type (_, _) eq =
  | Refl : ('a, 'a) eq (** Proof that ['a] and ['a] are equal. *)
(** [('a, 'b) eq] is the proposition that ['a] and ['b] are equal. *)

type 'sort ar = Arity of 'sort [@@ocaml.unbox]
(** A helper type for specifying arities. *)

type 'sort va = Valence of 'sort [@@ocaml.unbox]
(** A helper type for specifying valences. *)

(** The {i arity} of an operator typically consists of a sequence of sorts
    {i s{_ 1}, ..., s{_ n}} describing the operator's parameters, and the sort
    {i s} that the operator belongs to. The arity usually takes the form
    {i s{_ 1} × ... × s{_ n} → s}.

    Abstract binding trees record variables that are bound in the scope of a
    term. Therefore, operands have a {i valence}, which lists the sorts of the
    variables bound in the operand in addition to the sort of the operand
    itself. The valence takes the form {i s{_ 1} × ... × s{_ k} → s}, where
    {i s{_ 1}, ..., s{_ k}} are the sorts of the variables and {i s} is the
    sort of the operand.

    As a result, the arity for an operator of an abstract binding tree really
    takes the form {i v{_ 1} × ... × v{_ n} → s} where each {i v{_ i}} is a
    valence.

    Throughout this library, the ['valence] type parameter takes the form
    ['s1 -> ... 'sk -> 's va] where each ['si] is the sort of a bound variable
    and ['s] is the sort of the operand, and the ['arity] type parameter takes
    the form ['v1 -> ... -> 'vn -> 's ar], where each ['vi] is a valence of
    of an operand and ['s] is the sort of the operator. *)

module type Sort = sig
  type 'sort t
  (** The type parameter is a phantom type that represents the sort. *)

  val equal
    : 'sort1 t -> 'sort2 t
    -> (('sort1, 'sort2) eq, ('sort1, 'sort2) eq -> 'any) Either.t
  (** Decides the equality of two sorts. Iff the sorts are equal, returns
      a proof that their types are equal. Iff the sorts are unequal, it
      returns a proof that their types are not equal. *)
end
(** A sort is the syntactic class that an operator belongs to. *)

module type Operator = sig
  type ('arity, 'sort) t
  (** The operator's type contains two phantom type parameters, the first for
      the operator's arity and the second for the operator's sort. *)

  val equal
    : ('arity1, 'sort) t -> ('arity2, 'sort) t -> ('arity1, 'arity2) eq option
  (** Checks the equality of two operators from the same sort. Iff the
      operators are equal, returns a proof that their arities are equal. *)

  val pp_print : Format.formatter -> (_, _) t -> unit
  (** Prettyprints the operator. *)
end
(** An operator is a function symbol. *)

module type Variable = sig
  type 'sort t
  (** A variable annotated by its sort. *)

  type 'sort sort
  (** A sort. *)

  val fresh : 'sort sort -> string -> 'sort t
  (** Generates a fresh variable of the given sort. The variable is unique
      from any other variable generated from the function. *)

  val sort : 'sort t -> 'sort sort
  (** Retrieves the sort of the variable. *)

  val name : _ t -> string
  (** Retrieves the name of the variable. *)

  val equal : 'sort1 t -> 'sort2 t -> ('sort1, 'sort2) eq option
  (** Checks two variables for equality. Iff the variables are equal, returns
      [Some proof] that their sorts are the same. *)
end

module type S = sig
  module Sort : Sort
  module Operator : Operator
  module Variable : Variable with type 'sort sort = 'sort Sort.t

  type 'valence t
  (** An abstract binding tree (ABT). ['valence] is a phantom type parameter
      representing the valence of the ABT. *)

  type ('arity, 'sort) operands =
    | [] : ('sort ar, 'sort) operands
    (** An empty list of operands. *)
    | (::) : 'valence t * ('arity, 'sort) operands -> ('valence -> 'arity, 'sort) operands
    (** An operand followed by a list of operands. *)
  (** A list of operands. *)

  type 'valence view =
    | Abs : 'sort Variable.t * 'valence t -> ('sort -> 'valence) view
    (** An abstractor, which binds a variable within a term. *)
    | Op
      : ('arity, 'sort) Operator.t * ('arity, 'sort) operands -> 'sort va view
    (** An operator applied to operands. *)
    | Var : 'sort Variable.t -> 'sort va view
    (** A variable. *)
  (** A view of an ABT.*)

  val abs : 'sort Variable.t -> 'valence t -> ('sort -> 'valence) t
  (** Constructs an abstractor ABT. *)

  val op : ('arity, 'sort) Operator.t -> ('arity, 'sort) operands -> 'sort va t
  (** Constructs an operation ABT. *)

  val var : 'sort Variable.t -> 'sort va t
  (** Constructs a variable ABT. *)

  val into : 'valence view -> 'valence t
  (** Constructs an ABT from a view. *)

  val out : 'valence t -> 'valence view
  (** Views an ABT. *)

  val subst
    : 'sort Sort.t
    -> ('sort Variable.t -> 'sort va t option)
    -> 'valence t
    -> 'valence t
  (** Applies a substitution to the ABT. *)

  val aequiv : 'valence t -> 'valence t -> bool
  (** Checks two ABTs for alpha-equivalence. Two ABTs are alpha-equivalent iff
      they are structurally equal up to renaming of bound variables. *)

  val pp_print : Format.formatter -> _ t -> unit
  (** Pretty-prints an ABT. *)
end
(** Output signature of the functor {!module:Make}. *)
