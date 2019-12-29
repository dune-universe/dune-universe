open Owl_symbolic
open Op

(** TODO: automaticall test all examples *)

(* 
let test_gemm () =
  let a = variable ~shape:[| 3; 4 |] "a" in
  let b = variable ~shape:[| 4; 5 |] "b" in
  (* the user need to know that the correct shape of c *)
  let c = variable ~shape:[| 3; 5 |] "c" in
  let x = gemm ~c a b in
  let g = SymGraph.make_graph [| x |] "sym_graph" in
  let y = ONNX_Engine.of_symbolic g in
  ONNX_Engine.save y "test.onnx"
*)

(*
let test_split () =
  let a = variable ~shape:[| 3; 5 |] "a" in
  let x = split ~axis:1 a [| 2; 3 |] in
  let g = SymGraph.make_graph x "sym_graph" in
  let y = ONNX_Engine.of_symbolic g in
  ONNX_Engine.save y "test.onnx"
*)

(*
let test_maxpool () =
  let a = variable ~shape:[| 2; 6; 6; 2 |] "a" in
  let x, y = maxpool ~strides:[| 2; 2 |] ~padding:Type.VALID a [| 2; 2 |] in
  let g = SymGraph.make_graph [| x; y |] "sym_graph" in
  let z = ONNX_Engine.of_symbolic g in
  ONNX_Engine.save z "test.onnx"
*)

(*
let test_concat () =
  let a = variable ~shape:[| 2; 2 |] "a" in
  let b = variable ~shape:[| 2; 1 |] "b" in
  let x = concat ~axis:1 [| a; b |] in
  let g = SymGraph.make_graph [| x |] "sym_graph" in
  let y = ONNX_Engine.of_symbolic g in
  ONNX_Engine.save y "test.onnx"
*)

let test_sum () =
  let a = variable ~shape:[| 3; 2 |] "a" in
  let b = variable ~shape:[| 3; 1 |] "b" in
  let c = variable ~shape:[| 3; 2 |] "c" in
  let x = sum [| a; b; c |] in
  let g = SymGraph.make_graph [| x |] "sym_graph" in
  let y = ONNX_Engine.of_symbolic g in
  ONNX_Engine.save y "test.onnx"


let _ = test_sum ()
