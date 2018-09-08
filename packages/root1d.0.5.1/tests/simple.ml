(* Exemplify the use of the module on simple functions. *)


let () =
  (* 1 is a double root *)
  let f x = (x -. 3.) *. (x -. 1.) *. (x -. 1.) in
  let x1 = Root1D.brent f 0. 1. in (* root on boundary *)
  let x2 = Root1D.brent f 0. 3.2 in
  Printf.printf "Roots: %g (err: %g) and %g (err: %g)\n"
                x1 (x1 -. 1.) x2 (x2 -. 3.)
