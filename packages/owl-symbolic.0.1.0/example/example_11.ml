(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_neural_graph

(* Squeezenet 
 * Difference with Owl model: the kernel order in conv2d and input; 
 * the SAME padding to SAME_UPPER ; concat axis:1 *)

let fire_module in_shape squeeze expand nn =
  let root =
    conv2d ~padding:VALID [| squeeze; in_shape; 1; 1 |] [| 1; 1 |] nn |> activation Relu
  in
  let left =
    conv2d ~padding:VALID [| expand; squeeze; 1; 1 |] [| 1; 1 |] root |> activation Relu
  in
  let right =
    conv2d ~padding:SAME_UPPER [| expand; squeeze; 3; 3 |] [| 1; 1 |] root
    |> activation Relu
  in
  concat ~axis:1 [| left; right |]


let make_network img_size =
  input [| 10; 3; img_size; img_size |]
  |> conv2d ~padding:VALID [| 64; 3; 3; 3 |] [| 2; 2 |]
  |> max_pool2d [| 3; 3 |] [| 2; 2 |] ~padding:VALID
  (* block 1 *)
  |> fire_module 64 16 64
  |> fire_module 128 16 64
  |> max_pool2d [| 3; 3 |] [| 2; 2 |] ~padding:VALID
  (* block 2 *)
  |> fire_module 128 32 128
  |> fire_module 256 32 128
  |> max_pool2d [| 3; 3 |] [| 2; 2 |] ~padding:VALID
  (* block 3 *)
  |> fire_module 256 48 192
  |> fire_module 384 48 192
  |> fire_module 384 64 256
  |> fire_module 512 64 256
  (* include top *)
  |> dropout 0.1
  |> conv2d ~padding:VALID [| 1000; 512; 1; 1 |] [| 1; 1 |]
  |> activation Relu
  |> global_avg_pool2d
  |> activation (Softmax 1)
  |> get_network


let _ =
  let nn = make_network 227 in
  let onnx_graph = Owl_symbolic_engine_onnx.of_symbolic nn in
  Owl_symbolic_engine_onnx.save onnx_graph "test.onnx"
