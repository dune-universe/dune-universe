type ('a, 'b) data =
  | DataNil
  | DataCons of 'a * 'b * ('a, 'b) data

and 'a seq =
| Nil
| Zero of     ('a * 'a) seq
| One of 'a * ('a * 'a) seq

[@@deriving visitors { variety = "iter"; polymorphic = ["a"] },
            visitors { variety = "map"; polymorphic = ["a"] },
            visitors { variety = "endo"; polymorphic = ["a"] },
            visitors { variety = "reduce"; polymorphic = ["a"] },
            visitors { variety = "mapreduce"; polymorphic = ["a"] },
            visitors { variety = "iter2"; polymorphic = ["a"] },
            visitors { variety = "map2"; polymorphic = ["a"] },
            visitors { variety = "reduce2"; polymorphic = ["a"] },
            visitors { variety = "mapreduce2"; polymorphic = ["a"] }]
