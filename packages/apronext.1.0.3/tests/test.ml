open Apronext

let env = Environmentext.make_s [||] [|"x"; "y"; "z"|]

let gens =
  [Generatorext.of_float_point env [0.;0.;0.];
   Generatorext.of_float_point env [0.;100.;50.] ;
   Generatorext.of_float_point env [50.;200.;100.] ;
   Generatorext.of_float_point env [100.;0.;150.]]

let polyhedron = Apol.of_generator_list gens

let test_poly () =
  Format.printf "%a\n%a\n" Apol.print polyhedron Apol.pp_print polyhedron


let _ =
  test_poly()
