(* Testing @name attributes on data constructors. *)

type foo =
  | A [@name "TA"]
  | B of int [@name "TB"]
  | C of int * int [@name "TC"]
[@@deriving visitors { variety = "map"; concrete = true },
            visitors { variety = "fold"; ancestors = ["VisitorsRuntime.map"] }]

let f (x : foo) =
  let o = object
    inherit [_] map
    method! visit_TA _env = B 0
    method! visit_TB _env x = B (x + 1)
    method! visit_TC _env x y = C (x, x + y)
  end in
  o # visit_foo () x

let () =
  assert (f A = B 0);
  assert (f (B 0) = B 1);
  assert (f (C (1, 1)) = C (1, 2));
  ()

let g (x : foo) : int =
  let o = object
    inherit [_] fold
    method build_TA _env = 42
    method build_TB _env x = x
    method build_TC _env x y = x + y
  end in
  o # visit_foo () x

let () =
  assert (g A = 42);
  assert (g (B 12) = 12);
  assert (g (C (1, 1)) = 2);
  ()
