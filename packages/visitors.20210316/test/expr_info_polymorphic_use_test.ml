open Expr_info_polymorphic
open Expr_info_polymorphic_use

let strip : 'a . 'a expr -> unit expr = strip
let number : 'a . 'a expr -> int expr = number

let mk node = { info = (); node }

let const i =
  mk (EConst i)

let add e1 e2 =
  mk (EAdd (e1, e2))

let e =
  add (const 1) (add (const 0) (const 3))

let e = number (strip e)

let () =
  assert (e.info = 0);
  match e.node with
  | EAdd (e1, e2) ->
      assert (e1.info = 1);
      assert (e2.info = 2)
  | _ ->
      assert false
