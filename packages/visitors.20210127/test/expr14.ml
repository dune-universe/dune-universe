open Hashcons
open Expr12 (* [oexpr] *)
open Expr13 (*  [expr] *)
open Expr08 (* [hexpr] *)

let import : expr -> hexpr =
  let v = object (self)
    inherit [_] omap
    method visit_'expr _env (E e) =
      h (self#visit_oexpr _env e)
  end in
  v # visit_'expr ()

let export : hexpr -> expr =
  let v = object (self)
    inherit [_] omap
    method visit_'expr _env (H { node = e; _ }) =
      E (self#visit_oexpr _env e)
  end in
  v # visit_'expr ()
