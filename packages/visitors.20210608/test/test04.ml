type 'info expr_node =
  | EConst of int
  | EAdd of 'info expr * 'info expr

and 'info expr =
  { info: 'info; node: 'info expr_node }

[@@deriving visitors { variety = "iter"; polymorphic = true },
            visitors { variety = "map"; polymorphic = true },
            visitors { variety = "endo"; polymorphic = true },
            visitors { variety = "reduce"; polymorphic = true },
            visitors { variety = "mapreduce"; polymorphic = true },
            visitors { variety = "iter2"; polymorphic = true },
            visitors { variety = "map2"; polymorphic = true },
            visitors { variety = "reduce2"; polymorphic = true },
            visitors { variety = "mapreduce2"; polymorphic = true }]
