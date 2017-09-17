open Core
open Gnuplot

let () =
  let pi = Float.(4.*.atan 1.) in
  let generate_noise () =
    (* Box-Muller transform *)
    let box_muller u1 u2 =
      let r = sqrt (-2. *. log u1) in
      let t = 2. *. pi *. u2 in
      Float.(r *. cos t, r *. sin t)
    in
    List.init 1000 ~f:(fun _ -> box_muller (Random.float 1.) (Random.float 1.))
  in
  let gp = Gp.create () in
  (* Scatter plot of a bivariate normal distribution. *)
  Gp.set gp ~use_grid:true ~title:"2D Gaussian noise";
  Gp.plot gp (Series.points_xy (generate_noise ()))
    ~range:(Range.XY (-4., 4., -4., 4.));
  Gp.close gp
