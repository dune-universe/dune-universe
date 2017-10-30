open Bigarray
open Lacaml.D
open Printf

let pi = 4. *. atan 1.

let mesh_square =
  let pt = Array2.of_array float64 fortran_layout
    [| [| 0.; 1.; 1.; 0. |];   (* x *)
       [| 0.; 0.; 1.; 1. |] |] (* y *) in
  let edges = Array2.of_array int fortran_layout
    [| [| 1; 2; 3; 4 |];   (* start *)
       [| 2; 3; 4; 1 |] |] (* end *) in
  let plsg = object
    inherit [_] Mesh.pslg fortran_layout
    method point = pt
    method segment = edges
  end in
  Easymesh.triangulate plsg ~max_area:0.03


let () =
  printf "The mesh has %i nodes.\n%!" (Array2.dim2 mesh_square#point);
  printf "Press 'q' on its window to quit the mesh display.%!";
  Mesh_display.display mesh_square;
  printf "\n";
