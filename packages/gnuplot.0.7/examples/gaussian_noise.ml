module Gp = Gnuplot

let () =
  let pi = 4.*.atan 1. in
  let generate_noise () =
    let st = Random.State.make_self_init () in
    (* Box-Muller transform *)
    let box_muller u1 u2 =
      let r = sqrt (-2. *. log u1) in
      let t = 2. *. pi *. u2 in
      r *. cos t, r *. sin t
    in
    Base.List.init 1000 ~f:(fun _ -> box_muller (Random.State.float st 1.) (Random.State.float st 1.))
  in
  let gp = Gp.create () in
  (* Scatter plot of a bivariate normal distribution. *)
  Gp.set gp ~use_grid:true ~title:"2D Gaussian noise";
  Gp.plot gp (Gp.Series.points_xy (generate_noise ()))
    ~range:(Gp.XY (-4., 4., -4., 4.));
  Unix.sleep 10;
  Gp.close gp
