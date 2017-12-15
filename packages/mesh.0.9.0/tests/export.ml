open Printf
open Bigarray

let pi = acos(-1.)

let pslg_disk =
  let n = 100 in
  let point = Array2.create float64 fortran_layout 2 n in
  let segment = Array2.create int fortran_layout 2 n in
  let dt = 2. *. pi /. float n in
  for i = 1 to n do
    let t = float(i - 1) *. dt in
    point.{1,i} <- cos t;
    point.{2,i} <- sin t;
    segment.{1,i} <- i;
    segment.{2,i} <- if i = n then 1 else i + 1;
  done;
  Mesh_triangle.pslg point segment

let disk, _ = Mesh_triangle.triangulate pslg_disk ~max_area:1e-3

(* Export to Matlab, Scilab, and Mathematica. *)
let () =
  let f x y =
    let r = x**2. +. y**2. in
    cos(10. *. r) /. (r +. 0.1) in
  let v = Array1.create float64 fortran_layout (Array2.dim2 disk#point) in
  for i = 1 to Array2.dim2 disk#point do
    v.{i} <- f disk#point.{1,i} disk#point.{2,i};
  done;
  let fname = Filename.concat (Filename.get_temp_dir_name()) "disk" in
  Mesh.scilab disk v (fname ^ "_scilab");
  Mesh.matlab disk v (fname ^ "_matlab");
  Mesh.mathematica disk v (fname ^ "Mathematica")
