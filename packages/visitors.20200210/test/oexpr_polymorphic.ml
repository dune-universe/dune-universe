(* Defining open expressions
   and closing them,
   in one go,
   in [polymorphic] mode. *)

type 'expr oexpr =
  | EConst of int
  | EAdd of 'expr * 'expr

and expr =
  E of expr oexpr [@@unboxed]

[@@deriving visitors { variety = "map"; polymorphic = true; concrete = true }]

let double : expr -> expr =
  let v = object
    inherit [_] map
    method! visit_EConst _ _env k =
      EConst (2 * k)
  end in
  v # visit_expr ()
