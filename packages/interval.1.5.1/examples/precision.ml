(* Ill conditioned function *)

open Interval

let f_I x y =
  I.(333.75 *. y**6
     + x**2 * (11. *. x**2 * y**2 - y**6 - 121. *. y**4 -. 2.0)
     + 5.5 *. y**8
     + x / (2. *. y))

let f x y =
  333.75 *. y**6.
  +. x**2. *. (11.0 *. x**2. *. y**2. -. y**6. -. 121. *. y**4. -. 2.0)
  +. 5.5 *. y**8.0
  +. x /. (2. *. y)

let g u v =
  Printf.printf "Computing f(%f, %f)\n" u v;
  let a = I.v u u and b = I.v v v in
  let x = f u v in
  let y = f_I a b in
  Printf.printf "f(x,y) = %e\nf(I) = %a\n" x I.pr y;
  let err = 100.0 *. I.width_high y /. (abs_float x) in
  Printf.printf "error (in percent) = %e\n" err;
  print_newline();;

let () =
  g 77617.0 33095.999;
  g 77617.0 33096.001;
  g 77617.0 33096.0
