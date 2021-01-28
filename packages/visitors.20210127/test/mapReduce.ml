type t = int * bool

and u = { x: t; y: t }

and expr =
  | A
  | B of t
[@@deriving visitors { variety = "mapreduce"  },
            visitors { variety = "mapreduce2" }]
