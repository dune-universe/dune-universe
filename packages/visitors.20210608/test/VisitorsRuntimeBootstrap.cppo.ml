type 'a option =
  | None
  | Some of 'a

and 'a ref =
  { mutable contents: 'a }

#if OCAML_VERSION >= (4, 03, 0)

and 'a list =
  | Nil
  | Cons of 'a * 'a list

and ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

#endif

[@@deriving
visitors { variety = "iter"; public = []; polymorphic = true; data = false; nude = true },
visitors { variety = "map"; public = []; polymorphic = true; data = false; nude = true },
visitors { variety = "endo"; public = []; polymorphic = true; data = false; nude = true },
visitors { variety = "reduce"; public = []; polymorphic = true; data = false; nude = true; ancestors = ["VisitorsRuntime.monoid"] },
visitors { variety = "mapreduce"; public = []; polymorphic = true; data = false; nude = true; ancestors = ["VisitorsRuntime.monoid"] },
visitors { variety = "iter2"; public = []; polymorphic = true; data = false; nude = true },
visitors { variety = "map2"; public = []; polymorphic = true; data = false; nude = true },
visitors { variety = "reduce2"; public = []; polymorphic = true; data = false; nude = true; ancestors = ["VisitorsRuntime.monoid"] },
visitors { variety = "mapreduce2"; public = []; polymorphic = true; data = false; nude = true; ancestors = ["VisitorsRuntime.monoid"] }
]
