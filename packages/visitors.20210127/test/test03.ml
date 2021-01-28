type ('var, 'binder) term =
  | TVar of 'var
  | TAbs of 'binder * ('var, 'binder) term
  | TApp of ('var, 'binder) term * ('var, 'binder) term
[@@deriving
     visitors { variety = "iter" },
     visitors { variety = "map" },
     visitors { variety = "reduce" },
     visitors { variety = "endo" },
     visitors { variety = "iter2" },
     visitors { variety = "map2" },
     visitors { variety = "reduce2" }
]

(* Nominal. *)

module StringSet = Set.Make(String)

let iter = object(self)
  inherit [_] iter
  (* Descending methods for local types. *)
  method! visit_TAbs env x t =
    let env = StringSet.add x env in
    self#visit_term env t
  (* Descending methods for nonlocal types. *)
  method visit_'binder _env _x = ()
  method visit_'var env x =
    if StringSet.mem x env then
      Printf.printf "%s is a bound variable.\n%!" x
    else
      Printf.printf "%s is a free variable.\n%!" x

end

let t : (_, _) term =
  TAbs ("x", TApp (TVar "x", TVar "y"))

let () =
  iter#visit_term StringSet.empty t

(* De Bruijn. *)

let iter = object(self)
  inherit [_] iter
  (* Descending methods for local types. *)
  method! visit_TAbs env _x t =
    let env = 1 + env in
    self#visit_term env t
  (* Descending methods for nonlocal types. *)
  method visit_'binder _env _x = ()
  method visit_'var env x =
    if x < env then
      Printf.printf "%d is a bound variable.\n%!" x
    else
      Printf.printf "%d is a free variable.\n%!" x

end

let t : (_, _) term =
  TAbs ((), TApp (TVar 0, TVar 1))

let () =
  iter#visit_term 0 t
