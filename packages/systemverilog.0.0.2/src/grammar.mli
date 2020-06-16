module Literal : sig
  include module type of Grammar_types.Literal
  val to_s : t -> string
end

module Signing : sig
  include module type of Grammar_types.Signing
  val to_s : t -> string
end

module IntegerVectorType : sig
  include module type of Grammar_types.IntegerVectorType
  val to_s : t -> string
end

module IntegerAtomType : sig
  include module type of Grammar_types.IntegerAtomType
  val to_s : t -> string
end

module DataType : sig
  include module type of Grammar_types.DataType
  val to_s : t -> string
end

module ParamAssignment : sig
  include module type of Grammar_types.ParamAssignment
  val to_s : t -> string
end

module LocalParam : sig
  include module type of Grammar_types.LocalParam
  val to_s : t -> string
end

module PackageItem : sig
  include module type of Grammar_types.PackageItem
  val print : t -> unit
end

module Description : sig
  include module type of Grammar_types.Description
  val print : t -> unit
end
