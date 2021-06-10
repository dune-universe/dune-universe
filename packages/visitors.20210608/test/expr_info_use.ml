open Expr_info

let strip (e : _ expr) : unit expr =
  let v = object
    inherit [_] map
    method visit_'info _env _info = ()
  end in
  v # visit_expr () e

let number (e : _ expr) : int expr =
  let v = object
    inherit [_] map
    val mutable count = 0
    method visit_'info _env _info =
      let c = count in count <- c + 1; c
  end in
  v # visit_expr () e
