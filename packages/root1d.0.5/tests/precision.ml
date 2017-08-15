(* Compare the various methods of root finding. *)

open Printf

let pi = acos(-1.)

let f a x = cos x -. a *. sqrt (sin x)

let df a x = -. sin x -. 0.5 *. a *. cos x /. sqrt (sin x)

let compare methods a =
  let get_root meth =
    let n = ref 0 in (* number of fun eval *)
    let x = meth (fun x -> incr n; f a x) 0. pi in
    !n, x in
  match List.map get_root methods with
  | [] -> ()
  | (n0, x0) :: sols ->
     (* Use [x0] as the reference solution. *)
     printf "a=%5g: %-11g [%d] " a x0 n0;
     List.iter (fun (n, x) ->
                let e = abs_float((x -. x0) /. x0) in
                printf "%-11g [%d] ε=%.2e  " x n e
               ) sols;
     printf "\n"

let () =
  let m = [(fun f a b -> Root1D.bisection f a b ~eps:1e-14);
           (fun f a b -> Root1D.brent f a b ~tol:1e-30);
           (fun f a b -> Root1D.illinois f a b ~eps:1e-14);
          ] in
  printf "         Bisection [#eval] Brent                       Illinois\n";
  List.iter (compare m) [1.; 2.; 3.; 4.; 5.; 10.; 20.; 40.; 60.; 100.;
                         300.; 500.; 1000.; 5000.; 10000.; 50000.]
