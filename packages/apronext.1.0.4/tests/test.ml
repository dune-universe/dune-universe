open Apronext

let env = Environmentext.make_s [||] [|"x"; "y"; "z"|]

let gens =
  [ Generatorext.of_float_point env [0.; 0.; 0.]
  ; Generatorext.of_float_point env [0.; 100.; 50.]
  ; Generatorext.of_float_point env [50.; 200.; 100.]
  ; Generatorext.of_float_point env [100.; 0.; 150.] ]

let polyhedron = Apol.of_generator_list gens

let test_poly () =
  Format.printf "%a\n%a\n" Apol.print polyhedron Apol.pp_print polyhedron

let test_lin () =
  let lc = Apron.Parser.lincons1_of_string env "x + y - 3 >= 0" in
  Format.printf "%a\n" Linconsext.pp_print lc

let _ = test_poly () ; test_lin ()
