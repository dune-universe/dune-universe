
let pi = acos(-1.)

let () =
  let f x = if x = 0. then 0. else x *. sin (1. /. x) in
  let xmin = -0.4 and xmax = 0.4 in
  let t0 = Curve_sampling.uniform f xmin xmax ~n:1000 in
  Curve_sampling.to_file t0 "osc0.dat";
  let t = Curve_sampling.fn f xmin xmax ~n:227 in
  Curve_sampling.to_file t "osc227.dat";
  let t = Curve_sampling.fn f xmin xmax ~n:389 in
  Curve_sampling.to_file t "osc389.dat";

  let f x = sin (1. /. x) in
  let t0 = Curve_sampling.uniform f xmin xmax ~n:1000 in
  Curve_sampling.to_file t0 "osc1.dat";
  let t = Curve_sampling.fn f xmin xmax ~n:391 in
  Curve_sampling.to_file t "osc2.dat";

  let t = Curve_sampling.fn sin (-42. *. pi) (42. *. pi) ~n:400 in
  Curve_sampling.to_file t "osc3.dat";

  let t = Curve_sampling.fn (fun x -> sin(42. *. x)) (-.pi) pi ~n:400 in
  Curve_sampling.to_file t "osc4.dat"
