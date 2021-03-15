open Picasso
open Colors
open Apronext

let env = Environmentext.make_s [||] [|"x1"; "x2"; "x3"; "x4"|]

let gens =
  [ Generatorext.of_float_point env [120.; 0.; 100.; 88.]
  ; Generatorext.of_float_point env [105.; 100.; 150.; 55.]
  ; Generatorext.of_float_point env [150.; 200.; 220.; 11.]
  ; Generatorext.of_float_point env [200.; 0.; 250.; 55.] ]

let polyhedron = Apol.of_generator_list gens |> Drawable.of_pol

let octagon = Aoct.of_generator_list gens |> Drawable.of_oct

let box = Abox.of_generator_list gens |> Drawable.of_box

let _ =
  let r =
    Rendering.create ~abciss:"x1" ~ordinate:"x2" ~title:"Test" 800. 800.
  in
  let r =
    Rendering.add_l r [(blue, polyhedron); (red, octagon); (green, box)]
  in
  to_svg r "file.svg" ;
  to_latex ~tikz_only:false r "file.tex" ;
  let r3 = Rendering3d.create ~abciss:"x1" ~ordinate:"x2" ~height:"x3" () in
  let r3 = Rendering3d.add_l r3 [(blue, polyhedron)] in
  to_obj r3 "file.obj" ; show r
