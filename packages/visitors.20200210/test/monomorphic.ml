type 'a t =
  | Leaf of 'a
  | Node of 'a t * ('a * 'a) t
[@@deriving visitors { variety = "map"; monomorphic = ["'env"] }]

let o = object
  inherit [_] map
  method! visit_Leaf visit_'a env x =
    let env = (env : int) in (* check that ['env] is not quantified *)
    Leaf (visit_'a env x)
end
