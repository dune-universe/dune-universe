class ['self] base = object (_ : 'self)
  method visit_real _env x = x
end

type cloud =
  | Point of (float[@name "real"]) * (float[@name "real"])
  | Clouds of cloud list
  [@@name "nuage"]
  [@@deriving visitors { variety = "map"; ancestors = ["base"] }]

#if OCAML_VERSION >= (4, 03, 0)

module List = struct

  type 'a mylist = 'a list =
    | []                     [@name "nil"]
    | (::) of 'a * 'a mylist [@name "cons"]
    [@@deriving visitors { variety = "map" }]

end

#endif
