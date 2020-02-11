open Expr12

type expr =
  E of expr oexpr [@@unboxed]

class ['self] map = object (self : 'self)
  inherit [_] omap
  method visit_'expr env (E e) =
    E (self#visit_oexpr env e)
end
