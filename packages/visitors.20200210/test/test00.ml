type u = Uber
 and point = u * u
[@@deriving
     visitors { variety = "iter" },
     visitors { variety = "map" },
     visitors { variety = "reduce" },
     visitors { variety = "endo" },
     visitors { variety = "iter2" },
     visitors { variety = "map2" },
     visitors { variety = "reduce2" }
]
