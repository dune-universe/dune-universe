(*
#load "bigarray.cma";;
#load "graphics.cma";;
#load "mesh.cma";;
*)

open Bigarray
open Printf

module M = Mesh_triangle

let pi = 4. *. atan 1.

(* Round [x] to the nearest integer *)
let round x = floor(x +. 0.5)

let triunsuitable x1 y1 x2 y2 x3 y3 area = area > 0.001

let mesh, _ = M.triangulate ~triunsuitable (* ~max_area:0.001 *)
  (object
    inherit [_] M.pslg fortran_layout
      (* Fortran layout: point coordinates in columns *)
    method point = Array2.of_array float64 fortran_layout
      [| [| 0.0;  1.0;  0.5 |];
         [| 0.0;  0.0;  1.0 |] |];
      (* Fortran layout: segment enpoints in columns *)
    method segment = Array2.of_array int fortran_layout
      [| [| 1;  2;  3 |];
         [| 2;  3;  1 |] |];
   end)

let () =
  Easymesh.write mesh (Filename.temp_dir_name ^ "/mesh");
  Mesh_display.display mesh;
  Mesh.LaTeX.save mesh "testmesh.tex"  ~edge:(fun _ -> Some 0x00FF00);

  printf "Band of original mesh: %i\n" (Mesh.band_height_P1 mesh);
  let mesh' = Mesh.cuthill_mckee mesh in
  printf "Band after CutHill-McKee algorithm: %i\n" (Mesh.band_height_P1 mesh');
  Mesh.LaTeX.save mesh' "testmesh1.tex";

  (* Save the graph of [f] on [m] so it can be displayed by Scilab.  *)
  let f x y = sin(pi *. (2. *. x -. y)) *. sin(pi *. y) in
  let pt = mesh#point in
  let z = Array1.create float64 fortran_layout (Array2.dim2 pt) in
  for i = 1 to Array2.dim2 pt do z.{i} <- f pt.{1,i} pt.{2,i} done;
  let sci = Filename.temp_dir_name ^ "/triangle.sci" in
  Mesh.scilab mesh z sci;
  printf "Run Scilab script with: exec('%s')\n" sci;
  let m = Filename.temp_dir_name ^ "/triangle.m" in
  Mesh.mathematica mesh z m;
  printf "Run Mathematica script with: \"<< %s\"\n" m;

  (* Round to 4 decimal places *)
  for i = 1 to Array2.dim2 pt do z.{i} <- round(1e4 *. z.{i}) *. 1e-4 done;

  List.iter (fun i -> printf "z.{%i} = %g\n" i z.{i}) [1;2;3; Array2.dim2 pt];
  let in_black l = List.map (fun l -> (l, 0x000000)) l in
  Mesh.LaTeX.level_curves ~boundary:(fun _ -> Some 0xFF0000)
    mesh z (in_black [-0.5; -0.2; 0.; 0.1; 0.5; 0.8; 0.95]) "levels.tex"



(* Local Variables: *)
(* compile-command: "make -k test.exe test.com" *)
(* End: *)
