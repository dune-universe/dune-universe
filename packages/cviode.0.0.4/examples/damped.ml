module M = Owl.Dense.Matrix.D

let damped_noforcing ((xs, _) : M.mat * M.mat) (_ : float) : M.mat = M.(xs *$ -1.0)

let damped_forcing beta omega ((xs, ps) : M.mat * M.mat) t =
  M.(damped_noforcing (xs, ps) 0.0 +$ (beta *. Owl.Maths.sin (omega *. t)))


let a _ = 1.0
let dt = 0.1

let plot_sol fname t sol1 sol2 =
  let open Owl_plplot in
  let h = Plot.create fname in
  let open Plot in
  set_foreground_color h 0 0 0;
  set_background_color h 255 255 255;
  set_title h fname;
  plot ~h ~spec:[ RGB (0, 0, 255); LineStyle 1 ] t (Owl.Mat.col sol1 0);
  plot ~h ~spec:[ RGB (0, 255, 0); LineStyle 1 ] t (Owl.Mat.col sol2 0);
  (* XXX: I could not figure out how to make the legend black instead of red *)
  legend_on h ~position:NorthEast [| "Contact 1st"; "Contact 2nd" |];
  output h


let () =
  let open Owl_ode.Types in
  let y0 = M.of_array [| -0.25 |] 1 1, M.of_array [| 0.75 |] 1 1 in
  let tspec = T2 { tspan = 0.0, 15.0; dt } in
  let module Contact1 =
    Cviode.D.Contact1_damped (struct
      let a = a
    end)
  in
  let module Contact2 =
    Cviode.D.Contact2_damped (struct
      let a = a
    end)
  in
  let t, sol1, _ = Owl_ode.Ode.odeint (module Contact1) damped_noforcing y0 tspec () in
  let _, sol2, _ = Owl_ode.Ode.odeint (module Contact2) damped_noforcing y0 tspec () in
  plot_sol "damped.png" t sol1 sol2;
  let y0 = M.of_array [| -0.284 |] 1 1, M.of_array [| -0.027 |] 1 1 in
  let tspec = T2 { tspan = 0.0, 50.0; dt } in
  let damped_forcing = damped_forcing 0.3 (Stdcompat.Float.pi /. 3.0) in
  let t, sol1, _ = Owl_ode.Ode.odeint (module Contact1) damped_forcing y0 tspec () in
  let _, sol2, _ = Owl_ode.Ode.odeint (module Contact2) damped_forcing y0 tspec () in
  plot_sol "forced.png" t sol1 sol2
