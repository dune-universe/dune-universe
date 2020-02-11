type expr =
  | EConst of int
  | EAdd of expr * expr

class virtual ['self] iter : object ('self)
  method visit_EAdd   : 'monomorphic. 'env -> expr -> expr -> unit
  method visit_EConst : 'monomorphic. 'env -> int -> unit
  method visit_expr   : 'monomorphic. 'env -> expr -> unit
end
