open Core
open Gnuplot
open List_utils

let normal_gen ~mu ~sigma =
  stage (fun () ->
    let u = Random.float 1.0 in
    (* Generation of standard normal random variables, Rao et al. *)
    let z = -. log (1. /. u -. 1.) /. 1.702 in
    mu +. sigma *. z
  )

let dW_gen ~n ~dt =
  let dW = unstage (normal_gen ~mu:0. ~sigma:(sqrt dt)) in
  stage (fun () -> List.init n ~f:(fun _ -> dW ()))

(* Generates paths of geometric Brownian motion. *)
let gbm_paths ~s0 ~r ~sigma ~t ~n ~m =
  let dt = t /. float n in
  let drift = (r -. 0.5 *. sigma**2.) *. dt in
  let generate = unstage (dW_gen ~n ~dt) in
  List.init m ~f:(fun _ ->
    generate ()
    |> List.map ~f:(fun dW -> drift +. sigma *. dW)
    |> scan ~init:0. ~f:(+.)
    |> List.mapi ~f:(fun i x -> float i *. dt, s0 *. exp x)
  )

let () =
  let paths = gbm_paths ~s0:100. ~r:0.05 ~sigma:0.2 ~t:10. ~n:1000 ~m:10 in
  let gp = Gp.create () in
  Gp.set gp ~title:"Ten simulated paths of GBM" ~use_grid:true;
  Gp.plot_many gp ~labels:(Labels.create ~x:"Time in years" ~y:"Price" ())
    (List.map paths ~f:(fun path -> Series.lines_xy path));
  Gp.close gp
