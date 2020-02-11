open Expr12 (* oexpr *)

open Hashcons

type hexpr =
  H of hexpr oexpr hash_consed [@@unboxed]

let table =
  create 128

let h (e : hexpr oexpr) : hexpr =
  H (hashcons table e)

class ['self] hmap = object (self : 'self)
  inherit [_] omap
  method visit_'expr env (H { node = e; _ }) =
    h (self#visit_oexpr env e)
end
