open Printf
open Bigarray
module M = Mesh_triangle
open Graphics


(* Rectangle *)
let pslg = object
  inherit [_] M.pslg fortran_layout
  method point = Array2.of_array float64 fortran_layout
    [| [| -1.;  2.; 2.; -1. |];
       [| -1.; -1.; 1.;  1. |]|]
  method segment = Array2.of_array int fortran_layout
    [| [| 1; 2; 3; 4 |];
       [| 2; 3; 4; 1 |] |]
end

let vec_of_fun mesh fu =
  let pt = mesh#point in
  let u = Array1.create float64 fortran_layout (Array2.dim2 pt) in
  for i = 1 to Array2.dim2 pt do
    u.{i} <- fu pt.{1,i} pt.{2,i}
  done;
  u

let mesh, _ = M.triangulate pslg ~max_area:0.005 ~debug:false

let () =
  let width = 900 and height = 600 in
  open_graph (sprintf " %ix%i-40+30" (width + 20) (height + 20));
  moveto 10 10;

  let psi = vec_of_fun mesh (fun x y -> x +. y) in
  Mesh_display.super_level mesh ~width ~height psi 0. 0x81af40;
  let psi = vec_of_fun mesh (fun x y -> (x -. 1.)**2. +. y**2. -. 0.25) in
  Mesh_display.sub_level mesh ~width ~height psi 0. 0x4078af;

  Mesh_display.draw mesh ~width ~height ~color:0xCCCCCC ~points:false;

  set_line_width 2;
  let psi = vec_of_fun mesh (fun x y -> x -. y) in
  Mesh_display.level_curves mesh ~width ~height psi [(0., 0xFF0000)];
  ignore(wait_next_event [Button_down; Key_pressed]);
  close_graph()
