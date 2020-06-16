module Literal = struct
  type t =
    | Int of int
    | String of string
    | Hex of string
    | Dec of string
    | Bin of string
    | Oct of string
end

module Signing = struct
  type t =
    | Signed
    | Unsigned
end

module IntegerVectorType = struct
  type t =
    | Bit
    | Logic
    | Reg
end

module IntegerAtomType = struct
  type t =
    | Byte
    | Shortint
    | Int
    | Longint
    | Integer
    | Time
end

module DataType = struct
  type t =
    | IntegerVectorType of IntegerVectorType.t * Signing.t option
    | IntegerAtomType of IntegerAtomType.t * Signing.t option
end

module ParamAssignment = struct
  type t = string * Literal.t option
end

module LocalParam = struct
  type t =
    | Implicit of ParamAssignment.t list
    | Typed of DataType.t * ParamAssignment.t list
end

module PackageItem = struct
  type t =
    | Localparam of LocalParam.t
end

module Description = struct
  type t =
    | Package of string * PackageItem.t list
end
