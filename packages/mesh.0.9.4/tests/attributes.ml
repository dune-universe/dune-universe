open Printf
open Bigarray

let square_pslg r1 r2 =
  (* Square ]-r1, r1[² inside ]-r2, r2[² *)
  let point =
    Array2.of_array float64 fortran_layout
                    [| [| -. r1;   r1; r1; -. r1;  -. r2;   r2; r2; -. r2 |];
                       [| -. r1; -. r1; r1;   r1;  -. r2; -. r2; r2;   r2 |] |]
  and point_attribute =
    Array2.of_array float64 fortran_layout
                    [| [| 1.; 1.; 1.; 1.;  2.; 2.; 2.; 2. |] |]
  and segment =
    Array2.of_array int fortran_layout
                    [| [| 1; 2; 3; 4;  5; 6; 7; 8 |];
                       [| 2; 3; 4; 1;  6; 7; 8; 5 |] |]
  and segment_marker =
    Array1.of_array int fortran_layout [| 2; 2; 2; 2;  3; 3; 3; 3 |]

  and region =
    Array2.of_array float64 fortran_layout
                    [| [| 0.;            r2 |];
                       [| 0.;            r2 |];
                       [| 1.;            2. |];
                       [| 0.01 *. r1**2.; 0.01 *. r2**2. |] |] in
  Mesh_triangle.pslg point ~point_attribute ~segment_marker segment ~region

let square r1 r2 =
  fst(Mesh_triangle.triangulate (square_pslg r1 r2)
                                ~debug:false
                                ~region_area:true)

let () =
  let m = square 1. 2. in
  let triangle_idx t =
    let att = m#triangle_attribute.{1,t} in
    Graphics.set_color (if att = 1. then Graphics.red
                        else if att = 2. then Graphics.blue
                        else Graphics.green);
    let txt = sprintf "%g" att in
    let dx, dy = Graphics.text_size txt in
    Graphics.rmoveto (- dx / 2) (- dy /2);
    Graphics.draw_string txt;
    Graphics.set_color Graphics.black in
  let point_idx i =
    (* if m#point_attribute.{1,i} <> 0. then ( *)
    (*   let c = if m#point_attribute.{1,i} = 1. then Graphics.red *)
    (*           else if m#point_attribute.{1,i} = 2. then Graphics.blue *)
    (*           else Graphics.green in *)
    (*   Graphics.set_color c; *)
    (*   Graphics.fill_circle (Graphics.current_x()) (Graphics.current_y()) 2 *)
    (* ) in *)
    if m#point_marker.{i} <> 0 then (
      let c = if m#point_marker.{i} = 2 then Graphics.red
              else if m#point_marker.{i} = 3 then Graphics.cyan
              else Graphics.green in
      Graphics.set_color c;
      Graphics.fill_circle (Graphics.current_x()) (Graphics.current_y()) 2
    ) in
  Mesh_display.display m ~point_idx ~triangle_idx
