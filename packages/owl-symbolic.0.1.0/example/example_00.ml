open Owl_symbolic

let _ =
  let x = Op.(sin (variable ~shape:[| 3; 3 |] "X")) in
  let g = SymGraph.make_graph [| x |] "sym_graph" in
  let y = ONNX_Engine.of_symbolic g in
  ONNX_Engine.save y "test.onnx"
