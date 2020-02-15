(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_neural_graph
open Owl_symbolic_types

(** ResNet 50 *)

let id_block input kernel_size filters stage block input_layer =
  let suffix = string_of_int stage ^ block ^ "_branch" in
  let conv_name = "res" ^ suffix in
  let bn_name = "bn" ^ suffix in
  let f1, f2, f3 = filters in
  let x =
    input_layer
    |> conv2d [| f1; input; 1; 1 |] [| 1; 1 |] ~padding:VALID ~name:(conv_name ^ "2a")
    |> normalisation ~name:(bn_name ^ "2a")
    |> activation Relu
    |> conv2d
         [| f2; f1; kernel_size; kernel_size |]
         [| 1; 1 |]
         ~padding:SAME_UPPER
         ~name:(conv_name ^ "2b")
    |> normalisation ~name:(bn_name ^ "2b")
    |> activation Relu
    |> conv2d [| f3; f2; 1; 1 |] [| 1; 1 |] ~padding:VALID ~name:(conv_name ^ "2c")
    |> normalisation ~name:(bn_name ^ "2c")
  in
  add x input_layer |> activation Relu


let conv_block input kernel_size filters strides stage block input_layer =
  let suffix = string_of_int stage ^ block ^ "_branch" in
  let conv_name = "res" ^ suffix in
  let bn_name = "bn" ^ suffix in
  let f1, f2, f3 = filters in
  let x =
    input_layer
    |> conv2d [| f1; input; 1; 1 |] strides ~padding:VALID ~name:(conv_name ^ "2a")
    |> normalisation ~name:(bn_name ^ "2a")
    |> activation Relu
    |> conv2d
         [| f2; f1; kernel_size; kernel_size |]
         [| 1; 1 |]
         ~padding:SAME_UPPER
         ~name:(conv_name ^ "2b")
    |> normalisation ~name:(bn_name ^ "2b")
    |> activation Relu
    |> conv2d [| f3; f2; 1; 1 |] [| 1; 1 |] ~padding:VALID ~name:(conv_name ^ "2c")
    |> normalisation ~name:(bn_name ^ "2c")
  in
  let shortcut =
    input_layer
    |> conv2d [| f3; input; 1; 1 |] strides ~name:(conv_name ^ "1")
    |> normalisation ~name:(bn_name ^ "1")
  in
  add x shortcut |> activation Relu


let resnet50 batch img_size nb_classes =
  input [| batch; 3; img_size; img_size |]
  |> zero_padding2d [| 3; 3; 3; 3 |]
  |> conv2d [| 64; 3; 7; 7 |] [| 2; 2 |] ~padding:VALID
  |> normalisation
  |> activation Relu
  |> max_pool2d [| 3; 3 |] [| 2; 2 |]
  |> conv_block 64 3 (64, 64, 256) [| 1; 1 |] 2 "a"
  |> id_block 256 3 (64, 64, 256) 2 "b"
  |> id_block 256 3 (64, 64, 256) 2 "c"
  |> conv_block 256 3 (128, 128, 512) [| 2; 2 |] 3 "a"
  |> id_block 512 3 (128, 128, 512) 3 "b"
  |> id_block 512 3 (128, 128, 512) 3 "c"
  |> id_block 512 3 (128, 128, 512) 3 "d"
  |> conv_block 512 3 (256, 256, 1024) [| 2; 2 |] 4 "a"
  |> id_block 1024 3 (256, 256, 1024) 4 "b"
  |> id_block 1024 3 (256, 256, 1024) 4 "c"
  |> id_block 1024 3 (256, 256, 1024) 4 "d"
  |> id_block 1024 3 (256, 256, 1024) 4 "e"
  |> id_block 1024 3 (256, 256, 1024) 4 "f"
  |> conv_block 1024 3 (512, 512, 2048) [| 2; 2 |] 5 "a"
  |> id_block 2048 3 (512, 512, 2048) 5 "b"
  |> id_block 2048 3 (512, 512, 2048) 5 "c"
  |> global_avg_pool2d ~name:"avg_pool"
  |> linear nb_classes ~name:"fc1000"
  |> activation (Softmax 1)
  |> get_network


let _ =
  let nn = resnet50 1 250 100 in
  let onnx_graph = Owl_symbolic_engine_onnx.of_symbolic nn in
  Owl_symbolic_engine_onnx.save onnx_graph "test.onnx"
