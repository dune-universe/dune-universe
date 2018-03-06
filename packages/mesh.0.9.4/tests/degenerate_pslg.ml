open Bigarray
module M = Mesh_triangle

let rectangle_pslg a1 b1 a2 b2 =
  let points = Array2.of_array float64 fortran_layout
                 [| [| a1; a2; a2; a1 |];
                    [| b1; b1; b2; b2 |] |] in
  let seg = Array2.of_array int fortran_layout [| [| 1; 2; 3; 4 |];
                                                  [| 2; 3; 4; 1 |] |] in
  M.pslg points seg

let rectangle =
  M.triangulate (rectangle_pslg 0. 1. 0. 1.) ~verbose:`VV
