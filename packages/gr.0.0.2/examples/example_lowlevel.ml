let wait_until_keypressed () =
  print_string "Press any key to exit";
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
      { termio with Unix.c_icanon = false }
  in
  let _ = input_char stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio

let () =
  let to_carray = Ctypes.(CArray.of_list double) in
  let captr = Ctypes.CArray.start in
  let xs = List.init 100 (fun _ -> -2.0 +. (4.0 *. Random.float 1.)) in
  let ys = List.init 100 (fun _ -> -2.0 +. (4.0 *. Random.float 1.)) in
  let zs =
    List.fold_left2
      (fun acc x y -> (x *. exp (-.((x ** 2.) +. (y ** 2.)))) :: acc)
      [] xs ys
    |> List.rev |> to_carray
  in
  let xs = to_carray xs in
  let ys = to_carray ys in
  let xa = Ctypes.(CArray.make double 200) in
  let ya = Ctypes.(CArray.make double 200) in
  let za = Ctypes.(CArray.make double @@ (200 * 200)) in
  let open Gr.Lowlevel in
  setviewport 0.1 0.95 0.1 0.9;
  setwindow (-2.0) 2.0 (-2.0) 2.0;
  setspace (-0.5) 0.5 0 90 |> ignore;
  setmarkersize 1.0;
  setmarkertype (-1);
  setcharheight 0.024;
  settextalign 2 0;
  settextfontprec 101 0;
  gridit 100 (captr xs) (captr ys) (captr zs) 200 200 (captr xa) (captr ya)
    (captr za);
  let h =
    List.init 20 (fun i -> -0.5 +. (float_of_int i /. 19.0)) |> to_carray
  in
  surface 200 200 (captr xa) (captr ya) (captr za) 5;
  contour 200 200 20 (captr xa) (captr ya) (captr h) (captr za) 0;
  polymarker 100 (captr xs) (captr ys);
  axes 0.25 0.25 (-2.0) (-2.0) 2 2 0.01;
  mathtex 0.5 0.9 {|\mbox{Attempt to plot tex stuff, e.g. } \int_0^1\sin(x)|};
  wait_until_keypressed ()
