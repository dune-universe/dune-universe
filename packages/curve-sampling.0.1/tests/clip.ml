open Gg

let () =
  let b = Box2.v (P2.v 0. 0.) (Size2.v 1. 1.) in
  let t = Curve_sampling.of_path
            [(0., -0.5); (1.5, 1.); (0.2, 0.5); (0.3, 1.5); (1., 0.6);
             (nan, nan); (-0.5, 0.5); (-1., 0.); (0.5, 0.5)] in
  Curve_sampling.to_file t "clip0.dat";
  Curve_sampling.to_latex t "clip0.tex";
  let t1 = Curve_sampling.clip t b in
  Curve_sampling.to_file t1 "clip1.dat";
  Curve_sampling.to_latex t1 "clip1.tex"

let () =
  let f x = (8. *. x**2. -. 10. *. x -. 1.) /. (1. -. 4. *. x)**2. in
  let s0 = Curve_sampling.uniform f 0. 5.3 in
  let s1 = Curve_sampling.fn f 0. 5.3 in
  let s2 = Curve_sampling.clip s1 (Box2.v (V2.v 0. (-2.)) (Size2.v 5. 2.)) in
  Curve_sampling.to_file s0 "clip2.dat";
  Curve_sampling.to_file s1 "clip3.dat";
  Curve_sampling.to_file s2 "clip4.dat"
