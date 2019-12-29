(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_graph
open Owl_symbolic_symbol

let noop = make_node NOOP [||]

(** Generator *)

let int ?name ?dtype value =
  let sym = Owl_symbolic_ops_generator.Int.create ?name ?dtype value in
  make_node (Int sym) [||]


let float ?name ?dtype value =
  let sym = Owl_symbolic_ops_generator.Float.create ?name ?dtype value in
  make_node (Owl_symbolic_symbol.Float sym) [||]


let complex ?name r i =
  let sym = Owl_symbolic_ops_generator.Complex.create ?name r i in
  make_node (Owl_symbolic_symbol.Complex sym) [||]


let pi ?name () =
  let sym = Owl_symbolic_ops_generator.Pi.create ?name () in
  make_node (Owl_symbolic_symbol.Pi sym) [||]


let tensor ?name t =
  let sym = Owl_symbolic_ops_generator.Tensor.create ?name t in
  make_node (Owl_symbolic_symbol.Tensor sym) [||]


let tensor_float ?name i =
  let t = Owl_symbolic_types.make_tensor ~dtype:SNT_Float ~flt_val:[| i |] [||] in
  tensor ?name t


let tensor_ints ?name ints =
  let t =
    Owl_symbolic_types.make_tensor ~dtype:SNT_Int64 ~int_val:ints [| Array.length ints |]
  in
  tensor ?name t


(** TODO: replace with symbol ONE | ZERO ? *)

let zeros ?name ?(dtype = Owl_symbolic_types.SNT_Float) shape =
  let l = Owl_symbolic_utils.nelt shape in
  let a = Array.make l 0. in
  let t = Owl_symbolic_types.make_tensor ~dtype ~flt_val:a shape in
  tensor ?name t


let ones ?name ?(dtype = Owl_symbolic_types.SNT_Float) shape =
  let l = Owl_symbolic_utils.nelt shape in
  let a = Array.make l 1. in
  let t = Owl_symbolic_types.make_tensor ~dtype ~flt_val:a shape in
  tensor ?name t


(* The shape and type are decided by initial value; 
 * if initial value not given, user need to specify them. 
 * Shape value default to scalar [||], and type default to SNT_Float. *)
let variable ?dtype ?shape ?init name =
  let s = Owl_symbolic_ops_generator.Variable.create ?dtype ?shape ?init name in
  make_node (Owl_symbolic_symbol.Variable s) [||]


let random_uniform ?dtype ?seed ?low ?high ?name shape =
  let s =
    Owl_symbolic_ops_generator.RandomUniform.create ?dtype ?seed ?low ?high ?name shape
  in
  make_node (Owl_symbolic_symbol.RandomUniform s) [||]


let random_normal ?dtype ?seed ?mean ?stddev ?name shape =
  let s =
    Owl_symbolic_ops_generator.RandomNormal.create ?dtype ?seed ?mean ?stddev ?name shape
  in
  make_node (Owl_symbolic_symbol.RandomNormal s) [||]


(** Math *)

let sin ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Sin.create ?name xn in
  make_node (Owl_symbolic_symbol.Sin s) [| x |]


let cos ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Cos.create ?name xn in
  make_node (Owl_symbolic_symbol.Cos s) [| x |]


let tan ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Tan.create ?name xn in
  make_node (Owl_symbolic_symbol.Tan s) [| x |]


let asin ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Asin.create ?name xn in
  make_node (Owl_symbolic_symbol.Asin s) [| x |]


let acos ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Acos.create ?name xn in
  make_node (Owl_symbolic_symbol.Acos s) [| x |]


let atan ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Atan.create ?name xn in
  make_node (Owl_symbolic_symbol.Atan s) [| x |]


let sinh ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Sinh.create ?name xn in
  make_node (Owl_symbolic_symbol.Sinh s) [| x |]


let cosh ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Cosh.create ?name xn in
  make_node (Owl_symbolic_symbol.Cosh s) [| x |]


let tanh ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Tanh.create ?name xn in
  make_node (Owl_symbolic_symbol.Tanh s) [| x |]


let asinh ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Asin.create ?name xn in
  make_node (Owl_symbolic_symbol.Asin s) [| x |]


let acosh ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Acos.create ?name xn in
  make_node (Owl_symbolic_symbol.Acos s) [| x |]


let atanh ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Atan.create ?name xn in
  make_node (Owl_symbolic_symbol.Atan s) [| x |]


let sqrt ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Sqrt.create ?name xn in
  make_node (Owl_symbolic_symbol.Sqrt s) [| x |]


let exp ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Exp.create ?name xn in
  make_node (Owl_symbolic_symbol.Exp s) [| x |]


let log ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Log.create ?name xn in
  make_node (Owl_symbolic_symbol.Log s) [| x |]


let erf ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Erf.create ?name xn in
  make_node (Owl_symbolic_symbol.Erf s) [| x |]


let sigmoid ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Sigmoid.create ?name xn in
  make_node (Owl_symbolic_symbol.Sigmoid s) [| x |]


let abs ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Abs.create ?name xn in
  make_node (Owl_symbolic_symbol.Abs s) [| x |]


let neg ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Neg.create ?name xn in
  make_node (Owl_symbolic_symbol.Neg s) [| x |]


let sign ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Sign.create ?name xn in
  make_node (Owl_symbolic_symbol.Sign s) [| x |]


let floor ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Floor.create ?name xn in
  make_node (Owl_symbolic_symbol.Floor s) [| x |]


let ceil ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Ceil.create ?name xn in
  make_node (Owl_symbolic_symbol.Ceil s) [| x |]


let round ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Round.create ?name xn in
  make_node (Owl_symbolic_symbol.Round s) [| x |]


let clip ?name ~min ~max x =
  let node1 = tensor_float min in
  let node2 = tensor_float max in
  let xn = Owl_symbolic_graph.name x in
  let n1 = Owl_symbolic_graph.name node1 in
  let n2 = Owl_symbolic_graph.name node2 in
  let s = Owl_symbolic_ops_math.Clip.create ?name xn n1 n2 in
  make_node (Owl_symbolic_symbol.Clip s) [| x; node1; node2 |]


let relu ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Relu.create ?name xn in
  make_node (Owl_symbolic_symbol.Relu s) [| x |]


let elu ?name ?alpha x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Elu.create ?name ?alpha xn in
  make_node (Owl_symbolic_symbol.Elu s) [| x |]


let leaky_relu ?name ?alpha x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.LeakyRelu.create ?name ?alpha xn in
  make_node (Owl_symbolic_symbol.LeakyRelu s) [| x |]


let softmax ?name ?axis x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Softmax.create ?name ?axis xn in
  make_node (Owl_symbolic_symbol.Softmax s) [| x |]


let softsign ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Softsign.create ?name xn in
  make_node (Owl_symbolic_symbol.Softsign s) [| x |]


let softplus ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_math.Softplus.create ?name xn in
  make_node (Owl_symbolic_symbol.Softplus s) [| x |]


let add ?name x y =
  let xn = Owl_symbolic_graph.name x in
  let yn = Owl_symbolic_graph.name y in
  let s = Owl_symbolic_ops_math.Add.create ?name xn yn in
  make_node (Owl_symbolic_symbol.Add s) [| x; y |]


let sub ?name x y =
  let xn = Owl_symbolic_graph.name x in
  let yn = Owl_symbolic_graph.name y in
  let s = Owl_symbolic_ops_math.Sub.create ?name xn yn in
  make_node (Owl_symbolic_symbol.Sub s) [| x; y |]


let mul ?name x y =
  let xn = Owl_symbolic_graph.name x in
  let yn = Owl_symbolic_graph.name y in
  let s = Owl_symbolic_ops_math.Mul.create ?name xn yn in
  make_node (Owl_symbolic_symbol.Mul s) [| x; y |]


let div ?name x y =
  let xn = Owl_symbolic_graph.name x in
  let yn = Owl_symbolic_graph.name y in
  let s = Owl_symbolic_ops_math.Div.create ?name xn yn in
  make_node (Owl_symbolic_symbol.Div s) [| x; y |]


let pow ?name x y =
  let xn = Owl_symbolic_graph.name x in
  let yn = Owl_symbolic_graph.name y in
  let s = Owl_symbolic_ops_math.Pow.create ?name xn yn in
  make_node (Owl_symbolic_symbol.Pow s) [| x; y |]


let modular ?name ?fmod x y =
  let xn = Owl_symbolic_graph.name x in
  let yn = Owl_symbolic_graph.name y in
  let s = Owl_symbolic_ops_math.Mod.create ?name ?fmod xn yn in
  make_node (Owl_symbolic_symbol.Mod s) [| x; y |]


let matmul ?name x y =
  let xn = Owl_symbolic_graph.name x in
  let yn = Owl_symbolic_graph.name y in
  let s = Owl_symbolic_ops_math.MatMul.create ?name xn yn in
  make_node (Owl_symbolic_symbol.MatMul s) [| x; y |]


let gemm ?name ?alpha ?beta ?transA ?transB ?c a b =
  let an = Owl_symbolic_graph.name a in
  let bn = Owl_symbolic_graph.name b in
  match c with
  | Some c ->
    let cn = Owl_symbolic_graph.name c in
    let s =
      Owl_symbolic_ops_math.Gemm.create ?name ?alpha ?beta ?transA ?transB ~c:cn an bn
    in
    make_node (Owl_symbolic_symbol.Gemm s) [| a; b; c |]
  | None   ->
    let s = Owl_symbolic_ops_math.Gemm.create ?name ?alpha ?beta ?transA ?transB an bn in
    make_node (Owl_symbolic_symbol.Gemm s) [| a; b |]


let max ?name xs =
  let xn = Array.map Owl_symbolic_graph.name xs in
  let s = Owl_symbolic_ops_math.Max.create ?name xn in
  make_node (Owl_symbolic_symbol.Max s) xs


let min ?name xs =
  let xn = Array.map Owl_symbolic_graph.name xs in
  let s = Owl_symbolic_ops_math.Min.create ?name xn in
  make_node (Owl_symbolic_symbol.Min s) xs


let sum ?name xs =
  let xn = Array.map Owl_symbolic_graph.name xs in
  let s = Owl_symbolic_ops_math.Sum.create ?name xn in
  make_node (Owl_symbolic_symbol.Sum s) xs


let mean ?name xs =
  let xn = Array.map Owl_symbolic_graph.name xs in
  let s = Owl_symbolic_ops_math.Mean.create ?name xn in
  make_node (Owl_symbolic_symbol.Mean s) xs


(** Reduction *)

let reduce_sum ?keepdims ?name x axes =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_reduction.ReduceSum.create ?keepdims ?name xn axes in
  make_node (Owl_symbolic_symbol.ReduceSum s) [| x |]


let reduce_max ?keepdims ?name x axes =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_reduction.ReduceMax.create ?keepdims ?name xn axes in
  make_node (Owl_symbolic_symbol.ReduceMax s) [| x |]


let reduce_min ?keepdims ?name x axes =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_reduction.ReduceMin.create ?keepdims ?name xn axes in
  make_node (Owl_symbolic_symbol.ReduceMin s) [| x |]


let reduce_mean ?keepdims ?name x axes =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_reduction.ReduceMean.create ?keepdims ?name xn axes in
  make_node (Owl_symbolic_symbol.ReduceMean s) [| x |]


let reduce_sum_square ?keepdims ?name x axes =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_reduction.ReduceSumSquare.create ?keepdims ?name xn axes in
  make_node (Owl_symbolic_symbol.ReduceSumSquare s) [| x |]


let reduce_prod ?keepdims ?name x axes =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_reduction.ReduceProd.create ?keepdims ?name xn axes in
  make_node (Owl_symbolic_symbol.ReduceProd s) [| x |]


let reduce_logsum ?keepdims ?name x axes =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_reduction.ReduceLogSum.create ?keepdims ?name xn axes in
  make_node (Owl_symbolic_symbol.ReduceLogSum s) [| x |]


let reduce_logsumexp ?keepdims ?name x axes =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_reduction.ReduceLogSumExp.create ?keepdims ?name xn axes in
  make_node (Owl_symbolic_symbol.ReduceLogSumExp s) [| x |]


let reduce_l1 ?keepdims ?name x axes =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_reduction.ReduceL1.create ?keepdims ?name xn axes in
  make_node (Owl_symbolic_symbol.ReduceL1 s) [| x |]


let reduce_l2 ?keepdims ?name x axes =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_reduction.ReduceL2.create ?keepdims ?name xn axes in
  make_node (Owl_symbolic_symbol.ReduceL2 s) [| x |]


(** Logical *)

let and_ ?name x y =
  let xn = Owl_symbolic_graph.name x in
  let yn = Owl_symbolic_graph.name y in
  let s = Owl_symbolic_ops_logical.And.create ?name xn yn in
  make_node (Owl_symbolic_symbol.And s) [| x; y |]


let or_ ?name x y =
  let xn = Owl_symbolic_graph.name x in
  let yn = Owl_symbolic_graph.name y in
  let s = Owl_symbolic_ops_logical.Or.create ?name xn yn in
  make_node (Owl_symbolic_symbol.Or s) [| x; y |]


let not_ ?name x y =
  let xn = Owl_symbolic_graph.name x in
  let yn = Owl_symbolic_graph.name y in
  let s = Owl_symbolic_ops_logical.Not.create ?name xn yn in
  make_node (Owl_symbolic_symbol.Not s) [| x; y |]


let xor ?name x y =
  let xn = Owl_symbolic_graph.name x in
  let yn = Owl_symbolic_graph.name y in
  let s = Owl_symbolic_ops_logical.Xor.create ?name xn yn in
  make_node (Owl_symbolic_symbol.Xor s) [| x; y |]


let greater ?name x y =
  let xn = Owl_symbolic_graph.name x in
  let yn = Owl_symbolic_graph.name y in
  let s = Owl_symbolic_ops_logical.Greater.create ?name xn yn in
  make_node (Owl_symbolic_symbol.Greater s) [| x; y |]


let less ?name x y =
  let xn = Owl_symbolic_graph.name x in
  let yn = Owl_symbolic_graph.name y in
  let s = Owl_symbolic_ops_logical.Less.create ?name xn yn in
  make_node (Owl_symbolic_symbol.Less s) [| x; y |]


let equal ?name x y =
  let xn = Owl_symbolic_graph.name x in
  let yn = Owl_symbolic_graph.name y in
  let s = Owl_symbolic_ops_logical.Equal.create ?name xn yn in
  make_node (Owl_symbolic_symbol.Equal s) [| x; y |]


let bitshift ?name ?rightshift x y =
  let xn = Owl_symbolic_graph.name x in
  let yn = Owl_symbolic_graph.name y in
  let s = Owl_symbolic_ops_logical.BitShift.create ?name ?rightshift xn yn in
  make_node (Owl_symbolic_symbol.BitShift s) [| x; y |]


(* TODO: the rules for this op are unclear *)
let equal_to ?name lhs rhs =
  let lhs_name = Owl_symbolic_graph.name lhs in
  let rhs_name = Owl_symbolic_graph.name rhs in
  let s = Owl_symbolic_ops_logical.EqualTo.create ?name lhs_name rhs_name in
  make_node (Owl_symbolic_symbol.EqualTo s) [| lhs; rhs |]


(** Tensor *)

let reshape ?name shape data =
  let snode = tensor_ints shape in
  let shp_name = Owl_symbolic_graph.name snode in
  let data_name = Owl_symbolic_graph.name data in
  let o = Owl_symbolic_ops_tensor.Reshape.create ?name shape data_name shp_name in
  let sym = Owl_symbolic_symbol.Reshape o in
  make_node sym [| data; snode |]


let split ?name ?axis x split =
  let num = Array.length split in
  assert (num > 0);
  (* build multiple outputs of split *)
  let split_name = Owl_symbolic_utils.node_name ?name "Split" in
  let id_nodes = Array.make num 0 in
  let output = Array.mapi (fun idx _ -> Printf.sprintf "%s_%d" split_name idx) id_nodes in
  (* create symbol *)
  let x_name = Owl_symbolic_graph.name x in
  let s =
    Owl_symbolic_ops_tensor.Split.create ~output ~name:split_name ?axis x_name split
  in
  let split_node = make_node (Owl_symbolic_symbol.Split s) [| x |] in
  (* create identity nodes *)
  Array.mapi
    (fun idx n ->
      let o = Owl_symbolic_ops_tensor.Identity.create ?name ~idx n in
      make_node (Owl_symbolic_symbol.Identity o) [| split_node |])
    output


let concat ?name ?axis xs =
  let xn = Array.map Owl_symbolic_graph.name xs in
  let s = Owl_symbolic_ops_tensor.Concat.create ?name ?axis xn in
  make_node (Owl_symbolic_symbol.Concat s) xs


let cast ?name x target =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_tensor.Cast.create ?name xn target in
  make_node (Owl_symbolic_symbol.Cast s) [| x |]


(* The order of paddings: [| axis0_start; axis1_start; ... axis0_end; axis1_end;...|] *)
(* Currently we only allow statically specified pads value *)
let pad ?name ?mode ?v x pads =
  (* change the order of pads to ONNX order; 
   * TODO: this step perhaps should be done in the engine. *)
  let l = Array.length pads in
  assert (l mod 2 = 0);
  let step = l / 2 in
  let p = Array.make l 0 in
  Array.iteri
    (fun i x -> if i mod 2 = 0 then p.(i / 2) <- x else p.((i / 2) + step) <- x)
    pads;
  let xn = Owl_symbolic_graph.name x in
  let pads_name = Owl_symbolic_utils.node_name "Tensor" in
  let pads_node = tensor_ints ~name:pads_name pads in
  match v with
  | Some v ->
    let vn = Owl_symbolic_graph.name v in
    let s = Owl_symbolic_ops_tensor.Pad.create ?name ?mode ~value:vn xn pads_name p in
    make_node (Owl_symbolic_symbol.Pad s) [| x; pads_node; v |]
  | None   ->
    let s = Owl_symbolic_ops_tensor.Pad.create ?name ?mode xn pads_name p in
    make_node (Owl_symbolic_symbol.Pad s) [| x; pads_node |]


let squeeze ?name ?axes x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_tensor.Squeeze.create ?name ?axes xn in
  make_node (Owl_symbolic_symbol.Squeeze s) [| x |]


let tile ?name x repeats =
  let r_name = Owl_symbolic_utils.node_name "Tensor" in
  let rep_node = tensor_ints repeats in
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_tensor.Tile.create ?name xn r_name repeats in
  make_node (Owl_symbolic_symbol.Tile s) [| x; rep_node |]


let shape ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_tensor.Shape.create ?name xn in
  make_node (Owl_symbolic_symbol.Shape s) [| x |]


let size ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_tensor.Size.create ?name xn in
  make_node (Owl_symbolic_symbol.Size s) [| x |]


let transpose ?name ?perm x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_tensor.Transpose.create ?name ?perm xn in
  make_node (Owl_symbolic_symbol.Transpose s) [| x |]


(* TODO: the inferface may be updated to accept starts/ends etc. as input nodes *)
(* TODO: what happen if only the second optional paramter is specified? *)
let slice ?name ?axes ?steps starts ends x =
  let start_node = tensor_ints starts in
  let end_node = tensor_ints ends in
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_tensor.Slice.create ?name ?axes ?steps starts ends xn in
  match axes, steps with
  | Some ax, Some st ->
    let ax_node = tensor_ints ax in
    let st_node = tensor_ints st in
    make_node
      (Owl_symbolic_symbol.Slice s)
      [| x; start_node; end_node; ax_node; st_node |]
  | Some ax, None ->
    let ax_node = tensor_ints ax in
    make_node (Owl_symbolic_symbol.Slice s) [| x; start_node; end_node; ax_node |]
  (* NOTE: currently we only allow specifiying steps if the axes is specified *)
  | _, _ -> make_node (Owl_symbolic_symbol.Slice s) [| x; start_node; end_node |]


let space_to_depth ?name blocksize x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_tensor.SpaceToDepth.create ?name xn blocksize in
  make_node (Owl_symbolic_symbol.SpaceToDepth s) [| x |]


let is_nan ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_tensor.IsNaN.create ?name xn in
  make_node (Owl_symbolic_symbol.IsNaN s) [| x |]


let non_zero ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_tensor.NonZero.create ?name xn in
  make_node (Owl_symbolic_symbol.NonZero s) [| x |]


let where ?name cond x y =
  let xn = Owl_symbolic_graph.name x in
  let yn = Owl_symbolic_graph.name y in
  let cn = Owl_symbolic_graph.name cond in
  let s = Owl_symbolic_ops_tensor.Where.create ?name cn xn yn in
  make_node (Owl_symbolic_symbol.Where s) [| cond; x; y |]


let scatter_elem ?name ?axis data indices updates =
  let datan = Owl_symbolic_graph.name data in
  let indicesn = Owl_symbolic_graph.name indices in
  let updatesn = Owl_symbolic_graph.name updates in
  let s =
    Owl_symbolic_ops_tensor.ScatterElements.create ?name ?axis datan indicesn updatesn
  in
  make_node (Owl_symbolic_symbol.ScatterElements s) [| data; indices; updates |]


let scatter_nd ?name data indices updates =
  let datan = Owl_symbolic_graph.name data in
  let indicesn = Owl_symbolic_graph.name indices in
  let updatesn = Owl_symbolic_graph.name updates in
  let s = Owl_symbolic_ops_tensor.ScatterND.create ?name datan indicesn updatesn in
  make_node (Owl_symbolic_symbol.ScatterND s) [| data; indices; updates |]


let gather_elem ?name ?axis data indices =
  let datan = Owl_symbolic_graph.name data in
  let indicesn = Owl_symbolic_graph.name indices in
  let s = Owl_symbolic_ops_tensor.GatherElements.create ?name ?axis datan indicesn in
  make_node (Owl_symbolic_symbol.GatherElements s) [| data; indices |]


let gather_nd ?name data indices =
  let datan = Owl_symbolic_graph.name data in
  let indicesn = Owl_symbolic_graph.name indices in
  let s = Owl_symbolic_ops_tensor.GatherND.create ?name datan indicesn in
  make_node (Owl_symbolic_symbol.GatherND s) [| data; indices |]


(** Neural Network *)

let conv ?name ?dim ?padding ?strides ?dilations ?bias input kernel =
  let i_name = Owl_symbolic_graph.name input in
  let k_name = Owl_symbolic_graph.name kernel in
  match bias with
  | Some b ->
    let b_name = Owl_symbolic_graph.name b in
    let s =
      Owl_symbolic_ops_nn.Conv.create
        ?name
        ?padding
        ?strides
        ?dilations
        ~bias_name:b_name
        i_name
        k_name
    in
    make_node (Owl_symbolic_symbol.Conv s) [| input; kernel; b |]
  | None   ->
    let s =
      Owl_symbolic_ops_nn.Conv.create
        ?name
        ?dim
        ?padding
        ?strides
        ?dilations
        i_name
        k_name
    in
    make_node (Owl_symbolic_symbol.Conv s) [| input; kernel |]


let conv_transpose ?name ?dim ?padding ?strides ?dilations ?bias input kernel =
  let i_name = Owl_symbolic_graph.name input in
  let k_name = Owl_symbolic_graph.name kernel in
  match bias with
  | Some b ->
    let b_name = Owl_symbolic_graph.name b in
    let s =
      Owl_symbolic_ops_nn.ConvTranspose.create
        ?name
        ?padding
        ?strides
        ?dilations
        ~bias_name:b_name
        i_name
        k_name
    in
    make_node (Owl_symbolic_symbol.ConvTranspose s) [| input; kernel; b |]
  | None   ->
    let s =
      Owl_symbolic_ops_nn.ConvTranspose.create
        ?name
        ?dim
        ?padding
        ?strides
        ?dilations
        i_name
        k_name
    in
    make_node (Owl_symbolic_symbol.ConvTranspose s) [| input; kernel |]


let maxpool ?name ?strides ?dilations ?padding input kernel_shp =
  (* build multiple outputs of split *)
  let n = Owl_symbolic_utils.node_name ?name "MaxPool" in
  let n1 = n ^ "_y" in
  let n2 = n ^ "_indices" in
  (* create symbol *)
  let input_name = Owl_symbolic_graph.name input in
  let s =
    Owl_symbolic_ops_nn.MaxPool.create
      ~name:n
      ~output:[| n1; n2 |]
      ?strides
      ?dilations
      ?padding
      input_name
      kernel_shp
  in
  let maxp = make_node (Owl_symbolic_symbol.MaxPool s) [| input |] in
  (* create identity nodes *)
  let o1 = Owl_symbolic_ops_tensor.Identity.create ~idx:0 ?name n1 in
  let o2 = Owl_symbolic_ops_tensor.Identity.create ~idx:1 ?name n2 in
  let out_1 = make_node (Owl_symbolic_symbol.Identity o1) [| maxp |] in
  let out_2 = make_node (Owl_symbolic_symbol.Identity o2) [| maxp |] in
  out_1, out_2


let avgpool
    ?name
    ?strides
    ?dilations
    ?padding
    ?ceil_mode
    ?count_include_pad
    input
    kernel_shp
  =
  let input_name = Owl_symbolic_graph.name input in
  let s =
    Owl_symbolic_ops_nn.AveragePool.create
      ?name
      ?strides
      ?dilations
      ?padding
      ?ceil_mode
      ?count_include_pad
      input_name
      kernel_shp
  in
  make_node (Owl_symbolic_symbol.AveragePool s) [| input |]


let batch_norm ?name ?eps ?momentum x scale bias mean var =
  (* build multiple outputs of split *)
  let n = Owl_symbolic_utils.node_name ?name "BatchNormalization" in
  let n1 = n ^ "_y" in
  let n2 = n ^ "_mean" in
  let n3 = n ^ "_var" in
  let n4 = n ^ "_saved_mean" in
  let n5 = n ^ "_saved_var" in
  (* create symbol *)
  let x_name = Owl_symbolic_graph.name x in
  let scale_name = Owl_symbolic_graph.name scale in
  let bias_name = Owl_symbolic_graph.name bias in
  let mean_name = Owl_symbolic_graph.name mean in
  let var_name = Owl_symbolic_graph.name var in
  let s =
    Owl_symbolic_ops_nn.BatchNormalization.create
      ~name:n
      ~output:[| n1; n2; n3; n4; n5 |]
      ?eps
      ?momentum
      x_name
      scale_name
      bias_name
      mean_name
      var_name
  in
  let bn_node =
    make_node (Owl_symbolic_symbol.BatchNormalization s) [| x; scale; bias; mean; var |]
  in
  (* create identity nodes *)
  let o1 = Owl_symbolic_ops_tensor.Identity.create ~idx:0 n1 in
  let o2 = Owl_symbolic_ops_tensor.Identity.create ~idx:1 n2 in
  let o3 = Owl_symbolic_ops_tensor.Identity.create ~idx:2 n3 in
  let o4 = Owl_symbolic_ops_tensor.Identity.create ~idx:3 n4 in
  let o5 = Owl_symbolic_ops_tensor.Identity.create ~idx:4 n5 in
  let out_1 = make_node (Owl_symbolic_symbol.Identity o1) [| bn_node |] in
  let out_2 = make_node (Owl_symbolic_symbol.Identity o2) [| bn_node |] in
  let out_3 = make_node (Owl_symbolic_symbol.Identity o3) [| bn_node |] in
  let out_4 = make_node (Owl_symbolic_symbol.Identity o4) [| bn_node |] in
  let out_5 = make_node (Owl_symbolic_symbol.Identity o5) [| bn_node |] in
  out_1, out_2, out_3, out_4, out_5


let instance_norm ?name ?eps input scale b =
  let i_name = Owl_symbolic_graph.name input in
  let s_name = Owl_symbolic_graph.name scale in
  let b_name = Owl_symbolic_graph.name b in
  let o = Owl_symbolic_ops_nn.InstanceNorm.create ?name ?eps i_name s_name b_name in
  make_node (Owl_symbolic_symbol.InstanceNorm o) [| input; scale; b |]


let dropout ?name ?ratio x =
  (* build multiple outputs of split *)
  let d_name = Owl_symbolic_utils.node_name ?name "Dropout" in
  let n1 = d_name ^ "_output" in
  let n2 = d_name ^ "_mask" in
  (* create symbol *)
  let x_name = Owl_symbolic_graph.name x in
  let output = [| n1; n2 |] in
  let s = Owl_symbolic_ops_nn.Dropout.create ~output ~name:d_name ?ratio x_name in
  let d_node = make_node (Owl_symbolic_symbol.Dropout s) [| x |] in
  (* create identity nodes *)
  let o1 = Owl_symbolic_ops_tensor.Identity.create ~idx:0 ?name n1 in
  let o2 = Owl_symbolic_ops_tensor.Identity.create ~idx:1 ?name n2 in
  let out1 = make_node (Owl_symbolic_symbol.Identity o1) [| d_node |] in
  let out2 = make_node (Owl_symbolic_symbol.Identity o2) [| d_node |] in
  out1, out2


let global_max_pool ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_nn.GlobalMaxPool.create ?name xn in
  make_node (Owl_symbolic_symbol.GlobalMaxPool s) [| x |]


let global_avg_pool ?name x =
  let xn = Owl_symbolic_graph.name x in
  let s = Owl_symbolic_ops_nn.GlobalAveragePool.create ?name xn in
  make_node (Owl_symbolic_symbol.GlobalAveragePool s) [| x |]


let flatten ?name ?axis data =
  let datan = Owl_symbolic_graph.name data in
  let s = Owl_symbolic_ops_nn.Flatten.create ?name ?axis datan in
  make_node (Owl_symbolic_symbol.Flatten s) [| data |]


let lstm ?name ?alpha ?beta ?clip ?activations ?direction ?input_forget hidden_size x w r =
  (* build multiple outputs of split *)
  let l_name = Owl_symbolic_utils.node_name ?name "LSTM" in
  let n0 = l_name ^ "_y" in
  let n1 = l_name ^ "_yh" in
  let n2 = l_name ^ "_yc" in
  (* create symbol *)
  let xn = Owl_symbolic_graph.name x in
  let wn = Owl_symbolic_graph.name w in
  let rn = Owl_symbolic_graph.name r in
  let output = [| n0; n1; n2 |] in
  let s =
    Owl_symbolic_ops_rnn.LSTM.create
      ~output
      ~name:l_name
      ?alpha
      ?beta
      ?clip
      ?activations
      ?direction
      ?input_forget
      hidden_size
      xn
      wn
      rn
  in
  let l_node = make_node (Owl_symbolic_symbol.LSTM s) [| x; w; r |] in
  (* create identity nodes *)
  let o0 = Owl_symbolic_ops_tensor.Identity.create ~idx:0 ?name n0 in
  let o1 = Owl_symbolic_ops_tensor.Identity.create ~idx:1 ?name n1 in
  let o2 = Owl_symbolic_ops_tensor.Identity.create ~idx:2 ?name n2 in
  let y = make_node (Owl_symbolic_symbol.Identity o0) [| l_node |] in
  let yh = make_node (Owl_symbolic_symbol.Identity o1) [| l_node |] in
  let yc = make_node (Owl_symbolic_symbol.Identity o2) [| l_node |] in
  y, yh, yc


let roi_align ?name ?mode ?height ?width ?ratio ?scale x rois batch_indices =
  let xn = Owl_symbolic_graph.name x in
  let rn = Owl_symbolic_graph.name rois in
  let bn = Owl_symbolic_graph.name batch_indices in
  let s =
    Owl_symbolic_ops_object_detection.RoiAlign.create
      ?mode
      ?height
      ?width
      ?ratio
      ?scale
      ?name
      xn
      rn
      bn
  in
  make_node (Owl_symbolic_symbol.RoiAlign s) [| x; rois; batch_indices |]


let seq_empty ?name ?dtype () =
  let s = Owl_symbolic_ops_sequence.SequenceEmpty.create ?name ?dtype () in
  make_node (Owl_symbolic_symbol.SequenceEmpty s) [||]
