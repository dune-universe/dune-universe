(* Testing @name attributes on data types. *)

(* Testing local types decorated with [@@name]. *)

module Point = struct

  type point = { x : coordinate; y : coordinate } [@@name "foo"]

  and coordinate = float [@@name "coord"]

  [@@deriving visitors { variety = "map"; concrete = true },
              visitors { variety = "fold"; ancestors = ["VisitorsRuntime.map"]}]

  let f (p : point) =
    let o = new map in
    o # visit_foo () p

  let () =
    assert (f { x = 0.; y = 0. } = { x = 0.; y = 0. });
    ()

  let g (p : point) : float =
    let o = object
      inherit [_] fold
      method build_coord _env x = x
      method build_foo _env x y = x +. y
    end in
    o # visit_foo () p

  let () =
    assert (g { x = 1.; y = 2. } = 3.);
    ()

end

type boolean = Vrai | Faux [@@name "condition"]
[@@deriving visitors { variety = "iter2"; concrete = true }]

let () =
  try
    new iter2 # fail_condition () Vrai Faux;
    assert false
  with VisitorsRuntime.StructuralMismatch ->
    ()

(* Testing nonlocal types decorated with [@name]. *)

type segment = { source: Point.point[@name "foo"]; destination: Point.point[@name "foo"] }
[@@deriving visitors { variety = "map"; concrete = true; nude = true; ancestors = ["Point.map"] }]
