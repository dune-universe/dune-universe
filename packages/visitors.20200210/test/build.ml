(* Testing @build attributes. *)

type foo =
  | A
  | B of int
  | C of foo * foo [@build fun x y -> C (x, y)]

and point = Point.point = private { x: int; y: int }
  [@@build Point.make]
  [@@deriving visitors { variety = "map" }]
