let two_pi = 2. *. acos(-1.)

let () =
  let xmin = -1. and xmax = 1.2 in
  let f x = if x <= 0.5 then abs_float x else 1. -. x in
  let t0 = Curve_sampling.uniform f xmin xmax ~n:1000 in
  Curve_sampling.to_file t0 "abs0.dat";
  let t = Curve_sampling.fn f xmin xmax ~n:40 in
  Curve_sampling.to_file t "abs.dat";
  Curve_sampling.Internal.write_segments t "abs_s.dat";

  let f x = abs_float(sin x) in
  let t0 = Curve_sampling.uniform f 0. two_pi ~n:1000 in
  Curve_sampling.to_file t0 "abs1.dat";
  let t = Curve_sampling.fn f 0. two_pi ~n:50 in
  Curve_sampling.to_file t "abs2.dat";
  Curve_sampling.Internal.write_segments t "abs2_s.dat"
