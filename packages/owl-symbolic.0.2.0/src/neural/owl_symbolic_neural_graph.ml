(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_types
open Owl_symbolic_graph

(** Target: normalisation; padding *)

let init typ shape =
  let fan_in, fan_out = Owl_symbolic_utils.calc_fans shape in
  let r0 = sqrt (1. /. fan_in) in
  let r1 = sqrt (6. /. (fan_in +. fan_out)) in
  match typ with
  | Uniform (a, b)       -> Owl_symbolic_operator.random_uniform ~low:a ~high:b shape
  | Gaussian (mu, sigma) ->
    Owl_symbolic_operator.random_normal ~mean:mu ~stddev:sigma shape
  | Standard             -> Owl_symbolic_operator.random_uniform ~low:(-.r0) ~high:r0 shape
  | Tanh                 -> Owl_symbolic_operator.random_uniform ~low:(-.r1) ~high:r1 shape


let activation ?name act_typ input_node =
  match (act_typ : activation) with
  | Tanh      -> Owl_symbolic_operator.tanh ?name input_node
  | Sigmoid   -> Owl_symbolic_operator.sigmoid ?name input_node
  | Softmax a -> Owl_symbolic_operator.softmax ?name ~axis:a input_node
  | Relu      -> Owl_symbolic_operator.relu ?name input_node
  | _         -> failwith "unimplemented yet"


let input ?name ?dtype shape =
  let name = Owl_symbolic_utils.node_name ?name "Variable" in
  Owl_symbolic_operator.variable ?dtype ~shape name


let max_pool2d ?name ?(padding = VALID) kernel strides input_node =
  assert (Array.length kernel = 2);
  assert (Array.length strides = 2);
  let a, _ = Owl_symbolic_operator.maxpool ?name ~strides ~padding input_node kernel in
  a


(* TODO need to check the dimension? *)
let avg_pool2d ?name ?(padding = VALID) kernel strides input_node =
  assert (Array.length kernel = 2);
  assert (Array.length strides = 2);
  Owl_symbolic_operator.avgpool ?name ~strides ~padding input_node kernel


let global_max_pool2d ?name input_node =
  let n = Owl_symbolic_operator.global_max_pool ?name input_node in
  let shp = n |> Owl_graph.attr |> Owl_symbolic_symbol.out_shape in
  match shp.(0) with
  | Some s ->
    assert (Array.length s > 2);
    Owl_symbolic_operator.reshape [| s.(0); s.(1) |] n
  | None   -> failwith "global_max_pool2d: unspecified input shape"


let global_avg_pool2d ?name input_node =
  let n = Owl_symbolic_operator.global_avg_pool ?name input_node in
  let shp = n |> Owl_graph.attr |> Owl_symbolic_symbol.out_shape in
  match shp.(0) with
  | Some s ->
    assert (Array.length s > 2);
    Owl_symbolic_operator.reshape [| s.(0); s.(1) |] n
  | None   -> failwith "global_max_pool2d: unspecified input shape"


let dropout ?name ratio input_node =
  let a, _ = Owl_symbolic_operator.dropout ?name ~ratio input_node in
  a


let lambda (f : symbol -> symbol) (input_node : symbol) = f input_node

let fully_connected ?name ?(init_typ = Standard) outputs input_node =
  let in_shape = input_node |> Owl_graph.attr |> Owl_symbolic_symbol.out_shape in
  let shp =
    match in_shape.(0) with
    | Some s -> s
    | None   -> failwith "fully_connected: unspecified input shape"
  in
  let m = Array.fold_left (fun a b -> a * b) 1 (Array.sub shp 1 (Array.length shp - 1)) in
  let n = outputs in
  let w = init init_typ [| m; n |] in
  let b = Owl_symbolic_operator.zeros [| 1; n |] in
  let x = Owl_symbolic_operator.reshape [| shp.(0); m |] input_node in
  Owl_symbolic_operator.(add (matmul ?name x w) b)


(* Kernel : [|out_c; in_c; h; w|]; input: [|n; c; h; w|] *)
let conv2d ?name ?(padding = SAME_UPPER) ?(init_typ = Tanh) kernel strides input_node =
  assert (Array.length kernel = 4);
  assert (Array.length strides = 2);
  let kernel_node = init init_typ kernel in
  let bias = Owl_symbolic_operator.zeros [| kernel.(0) |] in
  Owl_symbolic_operator.conv ?name ~dim:2 ~padding ~strides ~bias input_node kernel_node


(* Kernel : [|in_c; out_c; h; w|]; input: [|n; c; h; w|] *)
let transpose_conv2d
    ?name
    ?(padding = SAME_UPPER)
    ?(init_typ = Tanh)
    kernel
    strides
    input_node
  =
  assert (Array.length kernel = 4);
  assert (Array.length strides = 2);
  let kernel_node = init init_typ kernel in
  Owl_symbolic_operator.conv_transpose
    ?name
    ~dim:2
    ~padding
    ~strides
    input_node
    kernel_node


let linear ?name ?(init_typ = Standard) outputs input_node =
  let in_shape = input_node |> Owl_graph.attr |> Owl_symbolic_symbol.out_shape in
  let shp =
    match in_shape.(0) with
    | Some s -> s
    | None   -> failwith "fully_connected: unspecified input shape"
  in
  let m = Array.fold_left (fun a b -> a * b) 1 (Array.sub shp 1 (Array.length shp - 1)) in
  let n = outputs in
  let w = init init_typ [| m; n |] in
  let b = Owl_symbolic_operator.zeros [| 1; n |] in
  let y = Owl_symbolic_operator.(add (matmul ?name input_node w) b) in
  y


(* TODO: the correctness of this layer needs to be thoroughly checked *)
let normalisation ?name ?_axis ?eps ?momentum input_node =
  let in_shape = input_node |> Owl_graph.attr |> Owl_symbolic_symbol.out_shape in
  let shp =
    match in_shape.(0) with
    | Some s -> s
    | None   -> failwith "normalisation: unspecified input shape"
  in
  assert (Array.length shp >= 4);
  let c = shp.(1) in
  let gamma = Owl_symbolic_operator.ones [| c |] in
  let beta = Owl_symbolic_operator.zeros [| c |] in
  let mu = Owl_symbolic_operator.zeros [| c |] in
  let var = Owl_symbolic_operator.ones [| c |] in
  let y, _, _, _, _ =
    Owl_symbolic_operator.batch_norm ?name ?eps ?momentum input_node gamma beta mu var
  in
  y


let zero_padding2d ?name padding input_node =
  assert (Array.length padding = 4);
  let p = Array.append [| 0; 0; 0; 0 |] padding in
  let v = Owl_symbolic_operator.float 0. in
  Owl_symbolic_operator.pad ?name ~mode:"constant" ~v input_node p


let concat = Owl_symbolic_operator.concat

let add = Owl_symbolic_operator.add

let flt = Owl_symbolic_operator.float

let tanh = Owl_symbolic_operator.tanh

let get_network ?name input_node =
  let name =
    match name with
    | Some n -> n
    | None   -> "sym_graph"
  in
  Owl_symbolic_graph.make_graph [| input_node |] name
