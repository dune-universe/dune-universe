(* Verify the Min1D functions *)

open Printf

let test fname f a b =
  printf "\nFor function %s\nin [%g,%g] max found is at\t%!" fname a b;
  let count = ref 0 in
  let x, fx = Max1D.brent (fun x -> incr count; f x) a b in
  printf "%.9e\nMax function value found\t%.4e\nNo. of evaluations\t\t%d\n%!"
    x fx !count

let () =
  test "-x^3 + 2*x + 5" (fun x -> -. x *. x *. x +. 2. *. x +. 5.) 0. 1.;
  printf "Exact min is at\t\t\t0.81650\n";

  let f x =
    let y = x *. x *. x -. 2. *. x -. 5. in -. (y *. y)  in
  test "-. (x^3 - 2*x - 5)^2" f 2. 3.;
  printf "Exact root is \t\t\t2.0945514815\n";

  let f3 x =
    let y = cos x -. x in 2. -. y *. y  in
  test "2 - (cos(x)-x)^2" f3 2. 3.;
  test "2 - (cos(x)-x)^2" f3 (-1.) 3.;

  test "-(sin(x)-x)^2 - 1" (fun x -> let y = sin x -. x in -. y *. y -. 1.)
    (-1.) 3.
