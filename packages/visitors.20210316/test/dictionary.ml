(* Examples of mixing the monomorphic and polymorphic modes. *)

(* with a monomorphic 'env : *)
type ('a, 'b) dictionary =
  | Empty
  | NonEmpty of 'a * 'b * ('a, 'b) dictionary
[@@deriving visitors { variety = "map"; polymorphic = ["'b"] }]

(* with a polymorphic 'env : *)
(* dubious though, since the method [visit_'a] cannot use [env] *)
type ('a, 'b) dictionary2 =
  | Empty
  | NonEmpty of 'a * 'b * ('a, 'b) dictionary2
[@@deriving visitors { name = "map2"; variety = "map"; polymorphic = ["'b"; "'env"] }]
