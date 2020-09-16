let env =  ["x";"y"]

let pts = [[0.;0.];[10.;20.];[20.;0.]]

let test_poly () =
  Apol.make_of_float_points env pts
  |> Format.printf "%a\n" Apol.print


let _ =
  test_poly()
