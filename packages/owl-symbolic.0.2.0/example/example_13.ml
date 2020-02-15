(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_neural_graph
open Owl_symbolic_types
open Owl_symbolic_infix

(** Fast Neural Style Transfer *)

(* In trans_conv2d the kernel order is change to inc,outc, h, w;
 * TODO: the padding of conv2d_trans_layer is set to VALID instead of SAME*, which leads to error. 
 * This leads of slight change of outputshape. The padding issue should be studied later. *)

let conv2d_layer ?(relu = true) kernel stride nn =
  let result = conv2d ~padding:SAME_UPPER kernel stride nn |> normalisation in
  match relu with
  | true -> result |> activation Relu
  | _    -> result


let conv2d_trans_layer kernel stride nn =
  transpose_conv2d ~padding:VALID kernel stride nn |> normalisation |> activation Relu


let residual_block wh nn =
  let tmp =
    conv2d_layer [| 128; 128; wh; wh |] [| 1; 1 |] nn
    |> conv2d_layer ~relu:false [| 128; 128; wh; wh |] [| 1; 1 |]
  in
  add nn tmp


let make_network batch h =
  input [| batch; 3; h; h |]
  |> conv2d_layer [| 32; 3; 9; 9 |] [| 1; 1 |]
  |> conv2d_layer [| 64; 32; 3; 3 |] [| 2; 2 |]
  |> conv2d_layer [| 128; 64; 3; 3 |] [| 2; 2 |]
  |> residual_block 3
  |> residual_block 3
  |> residual_block 3
  |> residual_block 3
  |> residual_block 3
  |> conv2d_trans_layer [| 128; 64; 3; 3 |] [| 2; 2 |]
  |> conv2d_trans_layer [| 64; 32; 3; 3 |] [| 2; 2 |]
  |> conv2d_layer ~relu:false [| 3; 32; 9; 9 |] [| 1; 1 |]
  |> lambda (fun x -> (tanh x * flt 150.) + flt 127.5)
  |> get_network


let _ =
  let nn = make_network 1 224 in
  let onnx_graph = Owl_symbolic_engine_onnx.of_symbolic nn in
  Owl_symbolic_engine_onnx.save onnx_graph "test.onnx"
