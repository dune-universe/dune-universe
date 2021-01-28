open Expr12
open Expr13
open Expr13double

let const k = E (EConst k)
let add e1 e2 = E (EAdd (e1, e2))

let rec eval (E e) =
  match e with
  | EConst k -> k
  | EAdd (e1, e2) -> eval e1 + eval e2

let e : expr =
  add (const 1) (const 2)

let () =
  (* should print: 3 then 6 *)
  Printf.printf "%d\n%d\n%!" (eval e) (eval (double e))

let omap : 'expr1 'expr2 . ('expr1 -> 'expr2) -> 'expr1 oexpr -> 'expr2 oexpr =
  fun f e ->
    let v = object
      inherit [_] omap
      method visit_'expr _env e = f e
    end in
    v # visit_oexpr () e
