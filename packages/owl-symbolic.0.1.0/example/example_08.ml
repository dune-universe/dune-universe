open Owl_symbolic
open Op
open Infix

let make_expr0 () =
  (* construct *)
  let x = variable "x_0" in
  let y =
    exp ((sin x ** float 2.) + (cos x ** float 2.))
    + (float 10. * (x ** float 2.))
    + exp (pi () * complex 0. 1.)
  in
  let expr = SymGraph.make_graph [| y |] "sym_graph" in
  (* to LaTeX string *)
  LaTeX_Engine.of_symbolic expr |> print_endline;
  expr


let make_expr1 () =
  (* construct *)
  let alpha = variable "\\alpha" in
  let beta = variable "\\beta" in
  let theta = variable "\\theta" in
  let y = sqrt (alpha + atan (int 6)) - tanh (abs (beta / theta)) in
  let expr = SymGraph.make_graph [| y |] "sym_graph" in
  LaTeX_Engine.of_symbolic expr |> print_endline;
  expr


let make_expr2 () =
  (* construct *)
  let x = variable "x_i" in
  let y = (int 6 / int 4 * x) + (int 2 * x) in
  let expr1 = SymGraph.make_graph [| y |] "sym_graph" in
  (* initial simplification *)
  let expr2 = Owl_symbolic_cas_canonical.canonical_form expr1 in
  (* print to html for debugging *)
  (* let s = Owl_symbolic_graph.to_dot expr in 
  let _ = Owl_io.write_file "example_08.dot" s in
  Sys.command "dot -Tpdf example_08.dot -o example_08.pdf" |> ignore; *)
  LaTeX_Engine.of_symbolic expr2 |> print_endline;
  let z = equal_to expr1.sym_nodes.(0) expr2.sym_nodes.(0) in
  SymGraph.make_graph [| z |] "sym_graph"


let make_expr3 () =
  let x = variable "x_i" in
  let y = (int 2 * x) + (int 9 / int 6 * x) in
  (* TODO: this doesn't work: (int 2 * x) + (x * (int 9 / int 6)) *)
  let expr1 = SymGraph.make_graph [| y |] "sym_graph" in
  let expr2 = Owl_symbolic_cas_canonical.canonical_form expr1 in
  LaTeX_Engine.of_symbolic expr2 |> print_endline;
  LaTeX_Engine.of_symbolic expr2 |> print_endline;
  let z = equal_to expr1.sym_nodes.(0) expr2.sym_nodes.(0) in
  SymGraph.make_graph [| z |] "sym_graph"


let _ =
  let exprs = [ make_expr0 (); make_expr1 (); make_expr2 (); make_expr3 () ] in
  LaTeX_Engine.html ~dot:true ~exprs "example_08.html"
