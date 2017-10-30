open Printf
open Bigarray
open Graphics
module M = Mesh_triangle

let pslg =
  object
    inherit [_] M.pslg fortran_layout
    method point = Array2.of_array float64 fortran_layout
                                   [| [| 0.; 1.; 1.; 0. |];
                                      [| 0.; 0.; 1.; 1. |] |]
    method segment = Array2.of_array int fortran_layout
                                     [| [| 1; 2; 3; 4 |];
                                        [| 2; 3; 4; 1 |] |]
  end


let square, _ = M.triangulate pslg ~max_area:0.2 ~debug:false
                              ~neighbor:true

let square', _ =
  let n_triangle = Array2.dim2 square#triangle in
  let triangle_area = Array1.create float64 fortran_layout n_triangle in
  Array1.fill triangle_area 10.;
  triangle_area.{1} <- 0.01;
  M.refine square ~triangle_area ~debug:false


let () =
  printf "Original mesh: neighbors\ntriangle: ";
  let n = Array2.dim2 square#neighbor in
  for t = 1 to n do printf "%3i" t done;
  printf "\n";
  for i = 1 to 3 do
    printf "    %4i: " i;
    for t = 1 to n do printf "%3i" square#neighbor.{i,t} done;
    printf "\n";
  done;
  flush stdout

let () =
  open_graph " 880x440-30+10";
  moveto 10 10;
  (try set_font "-*-helvetica-bold-r-*--20-*-*-*-p-*-*-*" with _ -> ());
  let point_idx i = set_color 0xFF0000;
                    draw_string(string_of_int i) in
  let triangle_idx i = set_color 0x00FF00;
                       draw_string(string_of_int i) in
  Mesh_display.draw square ~width:400 ~height:400 ~point_idx ~triangle_idx;
  moveto 460 10;
  Mesh_display.draw square' ~width:400 ~height:400 ~point_idx;

  ignore(wait_next_event [Button_down; Key_pressed]);
  close_graph()
