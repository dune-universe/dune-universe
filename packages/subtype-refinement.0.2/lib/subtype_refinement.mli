module type IValue = sig
  type t

  val value : t
end

module type IConstraint = sig
  type t

  val where : t -> bool
end

module type Subtype = sig
  type super
  type t = private super

  exception FailedDownCast of super

  val upcast   : t     -> super
  val downcast : super -> t
end

val refine : ('a -> bool) -> (module Subtype with type super = 'a)

module Refine (Constraint : IConstraint) : Subtype with type super = Constraint.t
module Singleton (Value : IValue)        : Subtype with type super = Value.t

(* END *)
