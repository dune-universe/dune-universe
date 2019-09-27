let wait_until_keypressed () =
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN { termio with Unix.c_icanon = false }
  in
  let _ = input_char stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio


let () =
  let open Owl in
  let xs = Arr.init [| 100 |] (fun _ -> -2.0 +. (4.0 *. Random.float 1.)) in
  let ys = Arr.init [| 100 |] (fun _ -> -2.0 +. (4.0 *. Random.float 1.)) in
  let zs = Arr.map2 (fun x y -> x *. exp (-.((x ** 2.) +. (y ** 2.)))) xs ys in
  let h = Arr.init [| 20 |] (fun i -> -0.5 +. (float_of_int i /. 19.0)) in
  let open Gr in
  (* Customise Style
     Note that we left 0.05 on the viewport ymax
     for the title *)
  set_viewport 0.1 0.95 0.1 0.9;
  set_window (-2.0) 2.0 (-2.0) 2.0;
  set_space (-0.5) 0.5 0 90 |> ignore;
  set_markersize 1.0;
  set_markertype SOLID_CIRCLE;
  set_char_height 0.024;
  set_text_align (Some CENTER) None;
  set_text_font_prec TIMES_ROMAN;
  (* Plot surface, contours and markers *)
  let xa, ya, za = gridit xs ys zs (200, 200) in
  surface ~options:CELL_ARRAY xa ya za;
  contour xa ya h za;
  polymarker xs ys;
  (* Plot the axes *)
  set_char_height 0.02;
  axes ~origin:(-2.0, -2.0) ~major:(2, 2) ~tick_size:0.01 0.25 0.25;
  (* Add the title *)
  set_char_height 0.025;
  math_tex (0.5, 0.9) {|\mbox{Attempt to plot tex stuff, e.g. } \int_0^1\sin(x)|};
  (* Wait until keypressed... *)
  wait_until_keypressed ()
