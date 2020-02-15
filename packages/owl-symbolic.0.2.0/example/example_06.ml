module G = Owl_computation_cpu_engine.Make (Owl_algodiff_primal_ops.S)
module A = Owl_algodiff_generic.Make (G)
include Owl_neural_generic.Make (G)
open Graph
open Owl_symbolic
module OWL_Engine = Owl_symbolic_engine_owl.Make (G)

(*
let loss =
  let network = make_mnist_network [|32;32;3|] in
  let xt = G.var_arr "xt" ~shape:[|100;32;32;3|] |> A.pack_arr in
  let yt = G.var_arr "yt" ~shape:[|100;10|] |> A.pack_arr in
  let yt', _ = Graph.(init network; forward network xt) in
  let loss = A.(Maths.((cross_entropy yt yt') / (pack_flt (Mat.row_num yt |> float_of_int)))) in
  let inputs = [| xt |> A.unpack_arr |> G.arr_to_node |] in
  let s0_outputs = [| loss |> A.unpack_elt |> G.elt_to_node |] in
  G.make_graph ~input:inputs ~output:s0_outputs "mnist_loss"
*)

let make_mnist_network input_shape =
  input input_shape
  |> normalisation ~decay:0.9
  |> conv2d [| 3; 3; 3; 32 |] [| 1; 1 |]
  |> activation Activation.Relu
  |> max_pool2d [| 2; 2 |] [| 2; 2 |] ~padding:VALID
  |> fully_connected 512
  |> activation Activation.Relu
  |> linear 10
  |> activation Activation.(Softmax 1)
  |> get_network


let loss =
  let network = make_mnist_network [| 32; 32; 3 |] in
  let xt = G.var_arr "xt" ~shape:[| 100; 32; 32; 3 |] |> A.pack_arr in
  let yt', _ =
    Graph.(
      init network;
      forward network xt)
  in
  let input = [| xt |> A.unpack_arr |> G.arr_to_node |] in
  let output = [| yt' |> A.unpack_arr |> G.arr_to_node |] in
  G.make_graph ~input ~output "mnist_loss"


let _ =
  Owl_io.write_file "mnist_loss.dot" (G.graph_to_dot loss);
  Sys.command "dot -Tpdf mnist_loss.dot -o mnist_loss.pdf" |> ignore


let loss_sym = OWL_Engine.to_symbolic loss

let loss_onnx = ONNX_Engine.of_symbolic loss_sym

let _ = ONNX_Engine.save loss_onnx "test.onnx"
