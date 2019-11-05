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

module Refine (Constraint : IConstraint) : Subtype with
  type super = Constraint.t
= struct
  type super = Constraint.t
  type t     = Constraint.t

  exception FailedDownCast of Constraint.t

  let upcast value = value

  let downcast value =
    if Constraint.where value then value else raise (FailedDownCast value)
end

let refine (type a) condition =
  let module Module = Refine (struct
    type t = a

    let where = condition
  end) in
  (module Module : Subtype with type super = a)

module Singleton (Value : IValue) = struct
  include Refine (struct
    type t = Value.t

    let where value = (value == Value.value)
  end)
end

(* END *)
