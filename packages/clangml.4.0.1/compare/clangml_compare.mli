module type S = sig
  include Set.OrderedType

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t
end

module Decl : S with type t = Clang.Decl.t

module Type : S with type t = Clang.Type.t

module Expr : S with type t = Clang.Expr.t

module Stmt : S with type t = Clang.Stmt.t

module Enum_constant : S with type t = Clang.Enum_constant.t

module Translation_unit : S with type t = Clang.Translation_unit.t
