open Owl

let of_array' a : Arr.arr = Arr.of_array a [| 2 |]

let rk4 x h y f =
  let ( + ) = ( +. ) in
  let ( * ) = ( *. ) in
  let ( $* ) = Arr.( $* ) in
  let k1 = h $* f x y in
  let k2 = h $* f (x + (0.5 * h)) Arr.(y + (0.5 $* k1)) in
  let k3 = h $* f (x + (0.5 * h)) Arr.(y + (0.5 $* k2)) in
  let k4 = h $* f (x + h) Arr.(y + k3) in
  (x + h, Arr.(y + ((k1 + ((k2 + k3) *$ 2.0) + k4) /$ 6.0)))

let damped_pendulum_deriv (gamma, length) _t state =
  let theta, omega = (Arr.get state [| 0 |], Arr.get state [| 1 |]) in
  of_array'
    [| omega; (~-.gamma *. omega) -. (9.81 /. length *. Maths.sin theta) |]

let pendulum t theta omega acceleration =
  let open Gr in
  Workstation.clear ();
  set_viewport 0. 1. 0. 1.;
  let x = of_array' [| 0.5; 0.5 +. (Maths.sin theta *. 0.4) |] in
  let y = of_array' [| 0.8; 0.8 -. (Maths.cos theta *. 0.4) |] in
  let open Arr in
  (* draw pivot point *)
  fillarea
    (Arr.of_array [| 0.46; 0.54; 0.54; 0.46 |] [| 4 |])
    (Arr.of_array [| 0.79; 0.79; 0.81; 0.81 |] [| 4 |]);
  polyline ~linewidth:1. ~coloridx:1 x y;
  (* draw rod *)
  polymarker ~markersize:5. ~markertype:SOLID_CIRCLE ~coloridx:86
    x.${[ 1 ]}
    y.${[ 1 ]};
  (* draw rod *)
  let v = 0.05 *. omega in
  (* show angular velocity *)
  set_linecolorindex 4;
  Lowlevel.drawarrow x.%{1} y.%{1}
    (x.%{1} +. (v *. Maths.cos theta))
    (y.%{1} +. (v *. Maths.sin theta));
  set_linecolorindex 2;
  let a = 0.05 *. acceleration in
  (* show angular acceleration *)
  Lowlevel.drawarrow x.%{1} y.%{1}
    (x.%{1} +. (a *. Maths.sin theta))
    (y.%{1} +. (v *. Maths.cos theta));
  set_text_font_prec TIMES_BOLD ~precision:STRING;
  set_char_height 0.032;
  set_text_colorindex 1;
  tex_text (0.05, 0.925) "Damped Pendulum" |> ignore;
  set_char_height 0.040;
  math_tex (0.4, 0.22) {|\omega=\dot{\theta}|};
  math_tex (0.4, 0.1) {|\dot{\omega}=-\gamma\omega-\frac{g}{l}sin(\theta)|};
  set_char_height 0.028;
  tex_text (0.05, 0.22) @@ Printf.sprintf "t:%7.2f" t |> ignore;
  tex_text (0.05, 0.16)
  @@ Printf.sprintf "\\theta:%7.2f" (theta /. Float.pi *. 180.)
  |> ignore;
  set_text_colorindex 4;
  tex_text (0.05, 0.10) @@ Printf.sprintf "\\omega:%7.2f" omega |> ignore;
  set_text_colorindex 2;
  tex_text (0.05, 0.04) @@ Printf.sprintf "y_{a}:%6.2f" acceleration |> ignore;
  Workstation.update ()

let theta = 70.0 (* initial angle *)

let gamma = 0.1 (* damping coefficient *)

let length = 1.0 (* pendulum length *)

let dt = 0.04

let () =
  let state = ref @@ of_array' [| theta *. Float.pi /. 180.; 0. |] in
  let t = ref 0. in
  while !t < 30. do
    let t', state' = rk4 !t dt !state (damped_pendulum_deriv (gamma, length)) in
    t := t';
    state := state';
    let theta, omega = (Arr.get !state [| 0 |], Arr.get !state [| 1 |]) in
    let acceleration =
      Maths.sqrt (2.0 *. 9.81 *. length *. (1. -. Maths.cos theta))
    in
    pendulum !t theta omega acceleration
  done
