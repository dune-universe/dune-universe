type hop = position[@opaque]

and position = {
    pos_fname : string[@opaque];
    pos_lnum : int * (int[@opaque]);
    pos_bol : int;
    pos_cnum : int;
    foo: (int[@opaque]) list;
  }
and foo =
  | A of (int[@opaque]) * int
  | B of bool
[@@deriving
     visitors { variety = "iter" },
     visitors { variety = "map" },
     visitors { variety = "reduce" },
     visitors { variety = "endo" },
     visitors { variety = "iter2" },
     visitors { variety = "map2" },
     visitors { variety = "reduce2" }
]
