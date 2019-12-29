(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_neural_graph
open Owl_symbolic_types

(** InceptionV3 *)

(* Note to specify the defautl value of padding in avgpool etc. *)

let conv2d_bn ?(padding = SAME_UPPER) kernel stride nn =
  conv2d ~padding kernel stride nn |> normalisation |> activation Relu


let mix_typ1 in_shape bp_size nn =
  let branch1x1 = conv2d_bn [| 64; in_shape; 1; 1 |] [| 1; 1 |] nn in
  let branch5x5 =
    nn
    |> conv2d_bn [| 48; in_shape; 1; 1 |] [| 1; 1 |]
    |> conv2d_bn [| 64; 48; 5; 5 |] [| 1; 1 |]
  in
  let branch3x3dbl =
    nn
    |> conv2d_bn [| 64; in_shape; 1; 1 |] [| 1; 1 |]
    |> conv2d_bn [| 96; 64; 3; 3 |] [| 1; 1 |]
    |> conv2d_bn [| 96; 96; 3; 3 |] [| 1; 1 |]
  in
  let branch_pool =
    nn
    |> avg_pool2d ~padding:SAME_UPPER [| 3; 3 |] [| 1; 1 |]
    |> conv2d_bn [| bp_size; in_shape; 1; 1 |] [| 1; 1 |]
  in
  concat ~axis:1 [| branch1x1; branch5x5; branch3x3dbl; branch_pool |]


let mix_typ3 nn =
  let branch3x3 = conv2d_bn [| 384; 288; 3; 3 |] [| 2; 2 |] ~padding:VALID nn in
  let branch3x3dbl =
    nn
    |> conv2d_bn [| 64; 288; 1; 1 |] [| 1; 1 |]
    |> conv2d_bn [| 96; 64; 3; 3 |] [| 1; 1 |]
    |> conv2d_bn [| 96; 96; 3; 3 |] [| 2; 2 |] ~padding:VALID
  in
  let branch_pool = max_pool2d [| 3; 3 |] [| 2; 2 |] ~padding:VALID nn in
  concat ~axis:1 [| branch3x3; branch3x3dbl; branch_pool |]


let mix_typ4 size nn =
  let branch1x1 = conv2d_bn [| 192; 768; 1; 1 |] [| 1; 1 |] nn in
  let branch7x7 =
    nn
    |> conv2d_bn [| size; 768; 1; 1 |] [| 1; 1 |]
    |> conv2d_bn [| size; size; 1; 7 |] [| 1; 1 |]
    |> conv2d_bn [| 192; size; 7; 1 |] [| 1; 1 |]
  in
  let branch7x7dbl =
    nn
    |> conv2d_bn [| size; 768; 1; 1 |] [| 1; 1 |]
    |> conv2d_bn [| size; size; 7; 1 |] [| 1; 1 |]
    |> conv2d_bn [| size; size; 1; 7 |] [| 1; 1 |]
    |> conv2d_bn [| size; size; 7; 1 |] [| 1; 1 |]
    |> conv2d_bn [| 192; size; 1; 7 |] [| 1; 1 |]
  in
  let branch_pool =
    nn
    |> avg_pool2d [| 3; 3 |] [| 1; 1 |] ~padding:SAME_UPPER
    |> conv2d_bn [| 192; 768; 1; 1 |] [| 1; 1 |]
  in
  concat ~axis:1 [| branch1x1; branch7x7; branch7x7dbl; branch_pool |]


let mix_typ8 nn =
  let branch3x3 =
    nn
    |> conv2d_bn [| 192; 768; 1; 1 |] [| 1; 1 |]
    |> conv2d_bn [| 320; 192; 3; 3 |] [| 2; 2 |] ~padding:VALID
  in
  let branch7x7x3 =
    nn
    |> conv2d_bn [| 192; 768; 1; 1 |] [| 1; 1 |]
    |> conv2d_bn [| 192; 192; 1; 7 |] [| 1; 1 |]
    |> conv2d_bn [| 192; 192; 7; 1 |] [| 1; 1 |]
    |> conv2d_bn [| 192; 192; 3; 3 |] [| 2; 2 |] ~padding:VALID
  in
  let branch_pool = max_pool2d [| 3; 3 |] [| 2; 2 |] ~padding:VALID nn in
  concat ~axis:1 [| branch3x3; branch7x7x3; branch_pool |]


let mix_typ9 input nn =
  let branch1x1 = conv2d_bn [| 320; input; 1; 1 |] [| 1; 1 |] nn in
  let branch3x3 = conv2d_bn [| 384; input; 1; 1 |] [| 1; 1 |] nn in
  let branch3x3_1 = branch3x3 |> conv2d_bn [| 384; 384; 1; 3 |] [| 1; 1 |] in
  let branch3x3_2 = branch3x3 |> conv2d_bn [| 384; 384; 3; 1 |] [| 1; 1 |] in
  let branch3x3 = concat ~axis:1 [| branch3x3_1; branch3x3_2 |] in
  let branch3x3dbl =
    nn
    |> conv2d_bn [| 448; input; 1; 1 |] [| 1; 1 |]
    |> conv2d_bn [| 384; 448; 3; 3 |] [| 1; 1 |]
  in
  let branch3x3dbl_1 = branch3x3dbl |> conv2d_bn [| 384; 384; 1; 3 |] [| 1; 1 |] in
  let branch3x3dbl_2 = branch3x3dbl |> conv2d_bn [| 384; 384; 3; 1 |] [| 1; 1 |] in
  let branch3x3dbl = concat ~axis:1 [| branch3x3dbl_1; branch3x3dbl_2 |] in
  let branch_pool =
    nn
    |> avg_pool2d ~padding:SAME_UPPER [| 3; 3 |] [| 1; 1 |]
    |> conv2d_bn [| 192; input; 1; 1 |] [| 1; 1 |]
  in
  concat ~axis:1 [| branch1x1; branch3x3; branch3x3dbl; branch_pool |]


let make_network batch img_size =
  input [| batch; 3; img_size; img_size |]
  |> conv2d_bn [| 32; 3; 3; 3 |] [| 2; 2 |] ~padding:VALID
  |> conv2d_bn [| 32; 32; 3; 3 |] [| 1; 1 |] ~padding:VALID
  |> conv2d_bn [| 64; 32; 3; 3 |] [| 1; 1 |]
  |> max_pool2d [| 3; 3 |] [| 2; 2 |] ~padding:VALID
  |> conv2d_bn [| 80; 64; 1; 1 |] [| 1; 1 |] ~padding:VALID
  |> conv2d_bn [| 192; 80; 3; 3 |] [| 1; 1 |] ~padding:VALID
  |> max_pool2d [| 3; 3 |] [| 2; 2 |] ~padding:VALID
  |> mix_typ1 192 32
  |> mix_typ1 256 64
  |> mix_typ1 288 64
  |> mix_typ3
  |> mix_typ4 128
  |> mix_typ4 160
  |> mix_typ4 160
  |> mix_typ4 192
  |> mix_typ8
  |> mix_typ9 1280
  |> mix_typ9 2048
  |> global_avg_pool2d
  |> linear 1000
  |> activation (Softmax 1)
  |> get_network


let _ =
  let nn = make_network 1 299 in
  let onnx_graph = Owl_symbolic_engine_onnx.of_symbolic nn in
  Owl_symbolic_engine_onnx.save onnx_graph "test.onnx"
