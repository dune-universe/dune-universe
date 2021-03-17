open Expr_info_polymorphic

let v = new map

let strip : _ expr -> unit expr =
  let visit_'info _env _info = () in
  fun e ->
    v # visit_expr visit_'info () e

let number : _ expr -> int expr =
  let visit_'info count _info =
    let c = !count in count := c + 1; c in
  fun e ->
    let count = ref 0 in
    v # visit_expr visit_'info count e
