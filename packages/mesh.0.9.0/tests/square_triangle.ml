open Bigarray
open Lacaml.D
open Printf

let pi = 4. *. atan 1.

let pslg =
  let pt = Array2.of_array float64 fortran_layout
    [| [| 0.; 1.; 1.; 0. |];   (* x *)
       [| 0.; 0.; 1.; 1. |] |] (* y *) in
  let edges = Array2.of_array int fortran_layout
    [| [| 1; 2; 3; 4 |];   (* start *)
       [| 2; 3; 4; 1 |] |] (* end *) in
  object
    inherit [_] Mesh_triangle.pslg fortran_layout
    method point = pt
    method segment = edges
  end


let () =
  let max_area = 0.0005 in
  let mesh = fst(Mesh_triangle.triangulate pslg ~max_area) in
  printf "The mesh has %i nodes.\n%!" (Array2.dim2 mesh#point);
  printf "Press 'q' on its window to quit the mesh display.%!";
  Mesh_graphics.display mesh;
  let triunsuitable x1 y1 x2 y2 x3 y3 area = area > max_area in
  Mesh_graphics.display (fst(Mesh_triangle.triangulate pslg ~triunsuitable));
  printf "\n";
