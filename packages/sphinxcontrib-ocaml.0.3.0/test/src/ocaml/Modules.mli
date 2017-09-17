module type MT = sig
  (** Doc for Signature *)
  module Signature: sig
    type t1

    (** Floating doc *)

    [@@@something.else "A floating attribute that has nothing to do with documentation"]

    type t2

    (**       Floating doc with many leading spaces (which should be removed),
    several lines::

        and a quoted, indented part (that should keep its indentation)
        with another line with trailing spaces (that should be removed)                 

    And a conclusion with many trailing spaces (which should be removed as well).       *)
  end
  (** Doc' for Signature *)

  (** Doc for AliasOfSignature *)
  module AliasOfSignature = Signature
  (** Doc' for AliasOfSignature *)

  (** Doc for Hidden *)
  module Hidden: sig
  end [@@autodoc.hide]
  (** Doc for Hidden *)

  (** Doc for SimpleFunctor *)
  module SimpleFunctor(N: sig
    (** Floating Doc in sig of SimpleFunctor.N (because I've not found a way to attach doc to a functor parameter) *)

    (** Doc for SimpleFunctor.N.n *)
    val n: int
    (** Doc' for SimpleFunctor.N.n *)
  end): sig
    (** Doc for SimpleFunctor.t *)
    type t
    (** Doc' for SimpleFunctor.t *)
  end
  (** Doc' for SimpleFunctor *)

  (** Doc for MultiFunctor *)
  module MultiFunctor(A: sig
    (** Doc for MultiFunctor.A.a *)
    type a
  end)(B: sig
    (** Doc for MultiFunctor.B.b *)
    type b
  end)(C: sig
    (** Doc for MultiFunctor.C.c *)
    type c
  end): sig
    type t
  end

  module FunctorOfFunctor(
    F: functor (A: sig
      type a
    end) -> sig
      type f
    end
  ): sig
    type ff
  end

  (** Doc for AliasOfFunctorOfFunctor *)
  module AliasOfFunctorOfFunctor = FunctorOfFunctor
  (** Doc' for AliasOfFunctorOfFunctor *)

  (** Doc for ModuleTypeOfAliasOfFunctor *)
  module ModuleTypeOfAliasOfFunctor: module type of AliasOfFunctorOfFunctor
  (** Doc' for ModuleTypeOfAliasOfFunctor *)

  (** Doc for ModuleType *)
  module type ModuleType = sig
    (** Doc for ModuleType.t *)
    type t
    (** Doc' for ModuleType.t *)

    (** Doc for v *)
    val v: int
    (** Doc' for v *)

    (** Doc for M *)
    module M: sig
      val w: int
    end
    (** Doc' for M *)

    (** Doc for MT *)
    module type MT = sig
      val x: int
    end
    (** Doc' for MT *)

    (** Floating doc in ModuleType *)
  end [@@autodoc.hide]
  (** Doc' for ModuleType *)

  (** Doc for Identifier *)
  module Identifier: ModuleType
  (** Doc' for Identifier *)

  (** Doc for WithType *)
  module WithType: ModuleType with type t = float
  (** Doc' for WithType *)

  (** Doc for ModuleTypeAlias *)
  module type ModuleTypeAlias = ModuleType [@@autodoc.hide]
  (** Doc' for ModuleTypeAlias *)

  (** Doc for IndirectIdentifier *)
  module IndirectIdentifier: ModuleTypeAlias
  (** Doc' for IndirectIdentifier *)

  module FunctorWithIdentifierParam(A: ModuleTypeAlias): sig end

  (** Doc for FunctorType *)
  module type FunctorType = functor(A: sig
    (** Doc for a *)
    type a
  end) -> sig
    (** Doc for t *)
    type t
  end [@@autodoc.hide]
  (** Doc' for FunctorType *)

  (** Doc for FunctorAsIdentifier *)
  module FunctorAsIdentifier: FunctorType
  (** Doc' for FunctorAsIdentifier *)

  (** Doc for Recursive1 *)
  module rec Recursive1: sig
    (** Doc for t *)
    type t =
      | A1 (** Doc for A1 *)
      | B1 of Recursive2.t (** Doc for B1 *)
    (** Doc' for t *)
  end
  (** Doc' for Recursive1 *)

  (** Doc for Recursive2 *)
  and Recursive2: sig
    (** Doc for t *)
    type t =
      | A2 (** Doc for A2 *)
      | B2 of Recursive1.t (** Doc for B2 *)
    (** Doc' for t *)
  end
  (** Doc' for Recursive2 *)

  (** Doc for Abstract *)
  module type Abstract [@@autodoc.hide]
  (** Doc' for Abstract *)

  (** Doc for AbstractAsIdentifier *)
  module AbstractAsIdentifier: Abstract
  (** Doc' for AbstractAsIdentifier *)
end

include MT

(** A reference to a module: :mod:`Modules.Signature`. *)
