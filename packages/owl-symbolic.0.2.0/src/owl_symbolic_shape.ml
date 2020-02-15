(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_symbol

(* If an input is None, it means the static shape checking is not possible, and returns None. *)

let infer_shape_00 _input_shapes = [| Some [||] |]

let infer_shape_01 input_shapes =
  match input_shapes.(0).(0) with
  | Some s -> [| Some Array.(copy s) |]
  | None   -> [| None |]


let infer_shape_03 input_shapes =
  let s0 = input_shapes.(0).(0) in
  let s1 = input_shapes.(1).(0) in
  match s0, s1 with
  | Some s0, Some s1 -> [| Some Owl_utils_infer_shape.(broadcast1 s0 s1) |]
  | _, _             -> [| None |]


let infer_shape_05 input_shapes repeats =
  match input_shapes.(0).(0) with
  | Some s -> [| Some Owl_utils_infer_shape.(tile s repeats) |]
  | None   -> [| None |]


let infer_shape_07 input_shapes axis =
  let s0 = Array.map (fun s -> s.(0)) input_shapes in
  if Array.exists
       (function
         | Some _ -> false
         | None   -> true)
       s0
  then [| None |]
  else (
    let s1 =
      Array.map
        (function
          | Some a -> a
          | None   -> failwith "infer_shape_07")
        s0
    in
    [| Some Owl_utils_infer_shape.(concatenate s1 axis) |])


let infer_shape_08 input_shapes axis splits =
  match input_shapes.(0).(0) with
  | Some s ->
    let s0 = Owl_utils_infer_shape.(split s axis splits) in
    Array.map (fun s -> Some s) s0
  | None   -> Array.(make (length splits) None)


let infer_shape_10 input_shapes axis keepdims =
  match input_shapes.(0).(0) with
  | Some s ->
    let l = Array.length s in
    assert (Array.for_all (fun a -> a >= ~-l && a <= l - 1) axis);
    [| Some Owl_symbolic_utils.(reduce s axis keepdims) |]
  | None   -> [| None |]


let infer_shape_11 input_shapes padding stride =
  let input_shape = input_shapes.(0).(0) in
  let kernel_shape = input_shapes.(1).(0) in
  match input_shape, kernel_shape with
  | Some input, Some kernel ->
    [| Some Owl_symbolic_utils.(conv1d input padding kernel stride) |]
  | _, _                    -> [| None |]


let infer_shape_12 input_shapes padding stride =
  let input_shape = input_shapes.(0).(0) in
  let kernel_shape = input_shapes.(1).(0) in
  match input_shape, kernel_shape with
  | Some input, Some kernel ->
    [| Some Owl_symbolic_utils.(conv2d input padding kernel stride) |]
  | _, _                    -> [| None |]


let infer_shape_13 input_shapes padding stride =
  let input_shape = input_shapes.(0).(0) in
  let kernel_shape = input_shapes.(1).(0) in
  match input_shape, kernel_shape with
  | Some input, Some kernel ->
    [| Some Owl_symbolic_utils.(conv3d input padding kernel stride) |]
  | _, _                    -> [| None |]


let infer_shape_14 input_shapes padding stride =
  let input_shape = input_shapes.(0).(0) in
  let kernel_shape = input_shapes.(1).(0) in
  match input_shape, kernel_shape with
  | Some input, Some kernel ->
    [| Some Owl_symbolic_utils.(transpose_conv2d input padding kernel stride) |]
  | _, _                    -> [| None |]


let infer_shape_15 input_shapes padding kernel stride =
  let input_shape = input_shapes.(0).(0) in
  match input_shape with
  | Some input -> [| Some Owl_utils_infer_shape.(conv1d input padding kernel stride) |]
  | _          -> [| None |]


let infer_shape_17 input_shapes padding kernel stride =
  let input_shape = input_shapes.(0).(0) in
  match input_shape with
  | Some input -> [| Some Owl_utils_infer_shape.(conv3d input padding kernel stride) |]
  | _          -> [| None |]


let infer_shape_19 input_shapes =
  let x_shape = input_shapes.(0).(0) in
  let y_shape = input_shapes.(1).(0) in
  match x_shape, y_shape with
  | Some s0, Some s1 -> [| Some Owl_utils_infer_shape.(dot s0 s1) |]
  | _, _             -> [| None |]


let infer_shape_20 input_shapes axis =
  match input_shapes.(0).(0) with
  | Some s ->
    let axis = List.map (fun i -> Owl_types.R_ (Array.of_list i)) axis |> Array.of_list in
    let axis = Owl_base_slicing.check_slice_definition axis s in
    [| Some Owl_base_slicing.(calc_slice_shape axis) |]
  | None   -> [| None |]


let infer_shape_21 input_shapes padding kernel stride =
  let input_shape = input_shapes.(0).(0) in
  match input_shape with
  | Some input -> [| Some Owl_symbolic_utils.(pool2d input padding kernel stride) |]
  | _          -> [| None |]


let infer_shape_24 input_shapes padding stride =
  let input_shape = input_shapes.(0).(0) in
  let kernel_shape = input_shapes.(1).(0) in
  match input_shape, kernel_shape with
  | Some input, Some kernel ->
    [| Some Owl_utils_infer_shape.(transpose_conv1d input padding kernel stride) |]
  | _, _                    -> [| None |]


let infer_shape_25 input_shapes padding stride =
  let input_shape = input_shapes.(0).(0) in
  let kernel_shape = input_shapes.(1).(0) in
  match input_shape, kernel_shape with
  | Some input, Some kernel ->
    [| Some Owl_utils_infer_shape.(transpose_conv3d input padding kernel stride) |]
  | _, _                    -> [| None |]


let infer_shape_31 input_shapes =
  let msg = "Owl_symbolic_shape: infer_shape_31." in
  let unpack = Owl_symbolic_utils.get_option_value msg in
  let flag = Array.exists (fun x -> x.(0) = None) input_shapes in
  if flag
  then [| None |]
  else (
    let broadcast_shp =
      Array.fold_left
        (fun accu shps ->
          let shp = shps.(0) |> unpack in
          Owl_utils_infer_shape.broadcast1 shp accu)
        [||]
        input_shapes
    in
    [| Some broadcast_shp |])


let infer_shape_32 input_shapes =
  match input_shapes.(0).(0) with
  | Some s -> [| Some [| Array.length s |] |]
  | None   -> [| None |]


let infer_shape_33 input_shapes =
  match input_shapes.(0).(0) with
  | Some _ -> [| Some [||] |]
  | None   -> [| None |]


let infer_shape_34 input_shapes =
  assert (Array.length input_shapes = 3);
  match input_shapes.(0).(0), input_shapes.(1).(0), input_shapes.(2).(0) with
  | Some cond, Some x, Some y -> [| Some (Owl_utils_infer_shape.broadcast2 cond x y) |]
  | _, _, _                   -> [| None |]


let infer_shape_35 input_shapes =
  match input_shapes.(0).(0) with
  | Some s ->
    let l = Array.length s in
    assert (l >= 3);
    let axes = Owl_utils_array.range 2 (l - 1) in
    [| Some Owl_utils_infer_shape.(reduce s axes) |]
  | None   -> [| None |]


let infer_shape_det input_shapes =
  match input_shapes.(0).(0) with
  | Some s ->
    let dim = Array.length s in
    assert (dim = 2 || dim = 3);
    if dim = 2
    then (
      assert (s.(0) = s.(1));
      [| Some [||] |])
    else (
      assert (s.(1) = s.(2));
      [| Some [| s.(0) |] |])
  | None   -> [| None |]


let infer_shape_gemm (x : Owl_symbolic_ops_math.Gemm.t) input_shapes =
  let l = Array.length input_shapes in
  assert (l = 2 || l = 3);
  match input_shapes.(0).(0), input_shapes.(1).(0) with
  | Some a_shp, Some b_shp ->
    assert (Array.length a_shp = 2);
    assert (Array.length b_shp = 2);
    let a_shp = if x.transA then [| a_shp.(1); a_shp.(0) |] else a_shp in
    let b_shp = if x.transB then [| b_shp.(1); b_shp.(0) |] else b_shp in
    assert (a_shp.(1) = b_shp.(0));
    [| Some [| a_shp.(0); b_shp.(1) |] |]
  | _, _                   -> [| None |]


let infer_shape_pad (x : Owl_symbolic_ops_tensor.Pad.t) input_shapes =
  assert (Array.length input_shapes >= 2);
  let return_shp =
    match input_shapes.(0).(0), input_shapes.(1).(0) with
    | Some data_shp, Some pads_shp ->
      assert (Array.length pads_shp = 1);
      let l = Array.length data_shp in
      assert (pads_shp.(0) = 2 * l);
      assert (Array.length x.p = 2 * l);
      let foo = Owl_utils_array.to_string string_of_int x.p in
      Owl_log.info "fuck2: %s" foo;
      (* we cannot do shape checking of a graph containing Pad ndoe
       if the pads value is dynamically got from nodes, not as arguments in x.p *)
      let s = Array.mapi (fun i d -> x.p.(i) + d + x.p.(i + l)) data_shp in
      [| Some s |]
    | _, _                         -> [| None |]
  in
  if Array.length input_shapes = 3
  then (
    match input_shapes.(2).(0) with
    | Some s ->
      assert (Array.length s = 0);
      return_shp
    | _      -> [| None |])
  else return_shp


(* NOTE: this function set default parameters of symbol, 
 * something other than shape checking *)
let infer_shape_transpose input_shapes (x : Owl_symbolic_ops_tensor.Transpose.t) =
  let perm = x.perm in
  match input_shapes.(0).(0) with
  | Some s ->
    let l = Array.length s in
    (match perm with
    | Some p ->
      assert (Array.length p = l);
      let p' = Array.copy p in
      Array.sort Stdlib.compare p';
      assert (p' = Owl_utils_array.range 0 (l - 1));
      [| Some Owl_utils_infer_shape.(transpose s p) |]
    | None   ->
      let p = Owl_utils_array.range 0 (l - 1) in
      Owl_utils_array.reverse p;
      x.perm <- Some p;
      [| Some Owl_utils_infer_shape.(transpose s p) |])
  | None   -> [| None |]


(* TODO: test *)
let infer_shape_slice input_shapes (x : Owl_symbolic_ops_tensor.Slice.t) =
  match input_shapes.(0).(0) with
  | Some s ->
    let dim = Array.length s in
    let axes =
      match x.axes with
      | Some a ->
        assert (Array.for_all (fun x -> x >= ~-dim && x <= dim - 1) a);
        a
      | None   -> Owl_utils_array.range 0 (dim - 1)
    in
    let steps =
      match x.steps with
      | Some x -> x
      | None   -> Array.make dim 1
    in
    let l = Array.length axes in
    assert (Array.length steps = l);
    assert (Array.length x.starts = l);
    assert (Array.length x.ends = l);
    (* full index of [start; end; step] list *)
    let index = Array.make dim [ 0 ] in
    let c = ref 0 in
    for i = 0 to dim - 1 do
      let idx =
        if Array.mem i axes
        then (
          let ret = [ x.starts.(!c); x.ends.(!c); steps.(!c) ] in
          c := !c + 1;
          ret)
        else [ 0; s.(i) - 1; 1 ]
      in
      index.(i) <- idx
    done;
    infer_shape_20 input_shapes (Array.to_list index)
  | None   -> [| None |]


let infer_shape_space_to_depth input_shapes block_size =
  match input_shapes.(0).(0) with
  | Some s ->
    assert (Array.length s = 4);
    let shp =
      [| s.(0); s.(1) * block_size * block_size; s.(2) / block_size; s.(3) / block_size |]
    in
    [| Some shp |]
  | None   -> [| None |]


let infer_shape_depth_to_space input_shapes block_size =
  match input_shapes.(0).(0) with
  | Some s ->
    assert (Array.length s = 4);
    let shp =
      [| s.(0)
       ; s.(1) / (block_size * block_size)
       ; s.(2) * block_size
       ; s.(3) * block_size
      |]
    in
    [| Some shp |]
  | None   -> [| None |]


(* TODO: update 1D and 3D case to use util function from symbolic, not base *)
let infer_shape_conv input_shapes (x : Owl_symbolic_ops_nn.Conv.t) =
  let l = x.dim in
  let padding =
    match x.auto_pad with
    | "VALID"      -> Owl_types.VALID
    | "SAME_UPPER" -> Owl_types.SAME
    | _            -> Owl_types.SAME
    (* TODO *)
  in
  if l = 1
  then infer_shape_11 input_shapes padding x.strides
  else if l = 2
  then infer_shape_12 input_shapes padding x.strides
  else if l = 3
  then infer_shape_13 input_shapes padding x.strides
  else failwith "Owl_symbolic_shape: illegal conv dimensions."


(* TODO: update 1D and 3D case to use util function from symbolic, not base *)

let infer_shape_conv_transpose input_shapes (x : Owl_symbolic_ops_nn.ConvTranspose.t) =
  let l = x.dim in
  let padding =
    match x.auto_pad with
    | "VALID"      -> Owl_types.VALID
    | "SAME_UPPER" -> Owl_types.SAME
    | _            -> Owl_types.SAME
    (* TODO *)
  in
  if l = 1
  then infer_shape_24 input_shapes padding x.strides
  else if l = 2
  then infer_shape_14 input_shapes padding x.strides
  else if l = 3
  then infer_shape_25 input_shapes padding x.strides
  else failwith "Owl_symbolic_shape: illegal conv_transpose dimensions."


(** TODO: currently the pads value is not used *)
let infer_shape_pool ~typ input_shapes kernel_shp strides auto_pad _pads =
  let l = Array.length kernel_shp in
  let shp =
    match input_shapes.(0).(0) with
    | Some i ->
      let ndim = Array.length i - 2 in
      assert (ndim = l);
      let padding = if auto_pad = "VALID" then Owl_types.VALID else Owl_types.SAME in
      if ndim = 1
      then infer_shape_15 input_shapes padding kernel_shp strides
      else if ndim = 2
      then infer_shape_21 input_shapes padding kernel_shp strides
      else if ndim = 3
      then infer_shape_17 input_shapes padding kernel_shp strides
      else failwith "Owl_symbolic_shape: illegal pooling dimensions."
    | None   -> [| None |]
  in
  match typ with
  | `max -> [| shp.(0); shp.(0) |]
  | `avg -> shp


let infer_shape_batch_normalization input_shapes =
  let shp_x = input_shapes.(0).(0) in
  let shp_scale = input_shapes.(1).(0) in
  let shp_b = input_shapes.(2).(0) in
  let shp_mean = input_shapes.(3).(0) in
  let shp_var = input_shapes.(4).(0) in
  match shp_x, shp_scale, shp_b, shp_mean, shp_var with
  | Some x, Some scale, Some b, Some mean, Some var ->
    let c = x.(1) in
    assert (Array.length scale = 1);
    assert (Array.length b = 1);
    assert (Array.length mean = 1);
    assert (Array.length var = 1);
    assert (c = scale.(0));
    assert (c = b.(0));
    assert (c = mean.(0));
    assert (c = var.(0));
    [| Some x; Some [||]; Some [||]; Some [||]; Some [||] |]
  | _, _, _, _, _ -> [| None; None; None; None; None |]


let infer_shape_squeeze input_shapes axes =
  let input_shape = input_shapes.(0).(0) in
  match input_shape, axes with
  | Some shp, Some ax ->
    let l = Array.length shp in
    Array.iter
      (fun a ->
        assert (a < l);
        if shp.(a) <> 1
        then failwith "infer_shape_squeeze: specified axis is not single-dim")
      ax;
    let new_shp =
      Owl_utils_array.filteri (fun i _ -> not (Array.exists (fun a -> a = i) ax)) shp
    in
    [| Some new_shp |]
  | Some shp, None    ->
    let new_shp = Owl_utils_array.filter (fun s -> s <> 1) shp in
    [| Some new_shp |]
  | _, _              -> [| None |]


let infer_shape_unsqueeze input_shapes axes =
  let input_shape = input_shapes.(0).(0) in
  match input_shape with
  | Some shp ->
    let l = Array.length shp in
    let uniq_shp = Owl_utils_array.unique shp in
    assert (uniq_shp = shp);
    let dim = l + Array.length axes in
    Array.iter
      (fun a ->
        (* require axis to be positive to avoid potential issues *)
        assert (a < dim && a >= 0))
      axes;
    let new_shp = Array.make dim 1 in
    let c = ref 0 in
    Array.iteri
      (fun i _ ->
        if not (Array.mem i axes)
        then (
          new_shp.(i) <- shp.(!c);
          c := !c + 1))
      new_shp;
    [| Some new_shp |]
  | _        -> [| None |]


let infer_shape_scatter_elements input_shapes =
  assert (Array.length input_shapes = 3);
  match input_shapes.(0).(0), input_shapes.(1).(0), input_shapes.(2).(0) with
  | Some data_shp, Some indices_shp, Some updates_shp ->
    assert (Array.length data_shp >= 1);
    assert (Array.(length data_shp = length indices_shp));
    assert (data_shp = updates_shp);
    [| Some data_shp |]
  | _, _, _ -> [| None |]


let infer_shape_scatter_nd input_shapes =
  assert (Array.length input_shapes = 3);
  match input_shapes.(0).(0), input_shapes.(1).(0), input_shapes.(2).(0) with
  | Some data_shp, Some indices_shp, Some updates_shp ->
    let r = Array.length data_shp in
    let q = Array.length indices_shp in
    let p = Array.length updates_shp in
    assert (r >= 1);
    assert (p = r + q - 1 - indices_shp.(q - 1));
    [| Some data_shp |]
  | _, _, _ -> [| None |]


let infer_shape_gather_elements input_shapes =
  assert (Array.length input_shapes = 2);
  match input_shapes.(0).(0), input_shapes.(1).(0) with
  | Some data_shp, Some indices_shp ->
    assert (Array.length data_shp >= 1);
    assert (Array.(length data_shp = length indices_shp));
    [| Some indices_shp |]
  | _, _ -> [| None |]


let infer_shape_instance_norm input_shapes =
  match input_shapes.(0).(0), input_shapes.(1).(0), input_shapes.(2).(0) with
  | Some data_shp, Some scale_shp, Some b_shp ->
    assert (Array.length data_shp >= 4);
    assert (Array.length scale_shp = 1);
    assert (Array.length b_shp = 1);
    assert (scale_shp.(0) = data_shp.(1));
    assert (b_shp.(0) = data_shp.(1));
    [| Some data_shp |]
  | _, _, _ -> [| None |]


let infer_shape_flatten input_shapes axis =
  match input_shapes.(0).(0) with
  | Some shp ->
    let r = Array.length shp in
    assert (axis < r && axis >= 0);
    let a = Owl_symbolic_utils.nelt (Array.sub shp 0 (axis + 1)) in
    let b = Owl_symbolic_utils.nelt (Array.sub shp (axis + 1) (r - axis - 1)) in
    [| Some [| a; b |] |]
  | None     -> [| None |]


(* TODO: consider all the other optioanl parameters *)
let infer_shape_lstm input_shapes =
  assert (Array.length input_shapes >= 3);
  match input_shapes.(0).(0), input_shapes.(1).(0), input_shapes.(2).(0) with
  | Some xshp, Some wshp, Some rshp ->
    assert (Array.length xshp = 3);
    let seq_length = xshp.(0) in
    let batch_size = xshp.(1) in
    let input_size = xshp.(2) in
    assert (Array.length wshp = 3);
    let num_directions = wshp.(0) in
    let hidden_size = wshp.(1) / 4 in
    assert (input_size = wshp.(2));
    assert (Array.length rshp = 3);
    assert (num_directions = rshp.(0));
    assert (hidden_size = rshp.(1) / 4);
    assert (hidden_size = rshp.(2));
    [| Some [| seq_length; num_directions; batch_size; hidden_size |]
     ; Some [| num_directions; batch_size; hidden_size |]
     ; Some [| num_directions; batch_size; hidden_size |]
    |]
  | _, _, _ -> [| None; None; None |]


(* TODO: consider all the other optioanl parameters *)
let infer_shape_rnn input_shapes =
  assert (Array.length input_shapes >= 3);
  match input_shapes.(0).(0), input_shapes.(1).(0), input_shapes.(2).(0) with
  | Some xshp, Some wshp, Some rshp ->
    assert (Array.length xshp = 3);
    let seq_length = xshp.(0) in
    let batch_size = xshp.(1) in
    let input_size = xshp.(2) in
    assert (Array.length wshp = 3);
    let num_directions = wshp.(0) in
    let hidden_size = wshp.(1) / 4 in
    assert (input_size = wshp.(2));
    assert (Array.length rshp = 3);
    assert (num_directions = rshp.(0));
    assert (hidden_size = rshp.(1) / 4);
    assert (hidden_size = rshp.(2));
    [| Some [| seq_length; num_directions; batch_size; hidden_size |]
     ; Some [| num_directions; batch_size; hidden_size |]
    |]
  | _, _, _ -> [| None; None; None |]


let infer_shape_roialign input_shapes (x : Owl_symbolic_ops_object_detection.RoiAlign.t) =
  assert (Array.length input_shapes = 3);
  match input_shapes.(0).(0), input_shapes.(1).(0), input_shapes.(2).(0) with
  | Some x_shp, Some rois_shp, Some ind_shp ->
    assert (Array.length x_shp = 4);
    assert (Array.length rois_shp = 2);
    assert (Array.length ind_shp = 1);
    let c = x_shp.(1) in
    let num_rois = rois_shp.(0) in
    assert (rois_shp.(1) = 4);
    assert (ind_shp.(0) = num_rois);
    [| Some [| num_rois; c; x.output_height; x.output_width |] |]
  | _, _, _ -> [| None |]


let infer_shape_axis input_shapes axis =
  match input_shapes.(0).(0) with
  | Some s ->
    let dim = Array.length s in
    assert (axis >= ~-dim && axis <= dim - 1);
    [| Some s |]
  | None   -> [| None |]


let infer_shape_ql_matmul input_shapes =
  assert (Array.length input_shapes = 8);
  let x_shape = input_shapes.(0).(0) in
  let y_shape = input_shapes.(3).(0) in
  match x_shape, y_shape with
  | Some s0, Some s1 -> [| Some Owl_utils_infer_shape.(dot s0 s1) |]
  | _, _             -> [| None |]


let infer_shape_compress input_shapes axis =
  match input_shapes.(0).(0), axis with
  | Some s, Some _ -> [| Some s |]
  (* when axis not specified, output shape depends on `condition` input *)
  | _, _ -> [| None |]


let infer_shape_revseq input_shapes =
  match input_shapes.(0).(0), input_shapes.(1).(0) with
  | Some s1, Some s2 ->
    assert (Array.length s1 >= 2);
    assert (Array.length s2 = 1);
    [| Some s1 |]
  | _, _             -> [| None |]


let infer_shape_seq_at input_shapes pos =
  assert (Array.length input_shapes.(0) > pos);
  [| input_shapes.(0).(pos) |]


let infer_shape_seq_cons input_shapes = Array.map (fun x -> x.(0)) input_shapes

let infer_shape_seq_insert input_shapes pos =
  let l = Array.length input_shapes.(0) in
  assert (l >= pos);
  Owl_utils_array.insert input_shapes.(0) [| input_shapes.(1).(0) |] pos


let infer_shape_seq_erase input_shapes pos =
  let l = Array.length input_shapes.(0) in
  assert (l >= pos);
  Owl_utils_array.remove input_shapes.(0) pos


let infer_shape_split_to_seq
    (input_shapes : int array option array array)
    (x : Owl_symbolic_ops_sequence.SplitToSequence.t)
  =
  match input_shapes.(0).(0) with
  | Some shp ->
    let l = Array.length shp in
    assert (x.axis < l && x.axis >= ~-l);
    let axis = if x.axis >= 0 then x.axis else l + x.axis in
    (match x.split_scalar, x.split_array with
    | Some n, None   ->
      let nsplit = shp.(axis) / n in
      let residual = shp.(axis) mod n in
      let normal_shp = Array.copy shp in
      normal_shp.(axis) <- n;
      let seq_shp = Array.make nsplit (Some normal_shp) in
      if residual <> 0
      then (
        let res_shp = Array.copy shp in
        res_shp.(axis) <- residual;
        Array.append seq_shp [| Some res_shp |])
      else seq_shp
    | None, Some a   ->
      Array.map
        (fun s ->
          let shp' = Array.copy shp in
          shp'.(axis) <- s;
          Some shp')
        a
    | None, None     ->
      let shp' = Array.copy shp in
      let shp' =
        if x.keepdims
        then (
          shp'.(axis) <- 1;
          shp')
        else Owl_utils_array.remove shp' axis
      in
      Array.make shp.(axis) (Some shp')
    | Some _, Some _ ->
      failwith "infer_shape_split_to_seq: split_scalar/array should not be set together.")
  | None     -> [| None |]


let infer_shape_concat_from_seq input_shapes axis new_axis =
  if not new_axis
  then infer_shape_07 input_shapes axis
  else (
    let s0 = Array.map (fun s -> s.(0)) input_shapes in
    if Array.exists
         (function
           | Some _ -> false
           | None   -> true)
         s0
    then [| None |]
    else (
      let s1 =
        Array.map
          (function
            | Some a -> a
            | None   -> failwith "infer_shape_concat_from_seq")
          s0
      in
      let s2 = Owl_utils_array.unique s1 in
      assert (Array.length s2 = 1);
      let shp = s2.(0) in
      let new_shp = Owl_utils_array.insert shp [| Array.length s1 |] axis in
      [| Some new_shp |]))


(* TODO: can the output shape be decided? *)
let infer_shape_non_max_suppression input_shapes =
  (* input: boxes, scores, max_output_boxes_per_class, iou_threshold, score_threshold *)
  assert (Array.length input_shapes <= 5);
  (match input_shapes.(0).(0), input_shapes.(1).(0) with
  | Some s1, Some s2 ->
    assert (Array.length s1 = 3);
    assert (Array.length s2 = 3);
    assert (s1.(2) = 4);
    assert (s1.(0) = s2.(0));
    assert (s1.(1) = s2.(2))
  | _, _             -> ());
  [| None |]


let infer_shape_resize input_shapes scales sizes =
  assert (Array.length input_shapes >= 3);
  (match input_shapes.(1).(0) with
  | Some s -> assert (Array.length s = 1)
  | None   -> failwith "infer_shape_resize: roi should not be none.");
  match input_shapes.(0).(0) with
  | Some shp ->
    (match scales, sizes with
    | Some s, None ->
      assert (Array.length s = Array.length shp);
      let new_shp =
        Array.map2
          (fun a b ->
            assert (a > 0.);
            int_of_float (a *. float_of_int b))
          s
          shp
      in
      [| Some new_shp |]
    | None, Some s -> [| Some s |]
    | _, _         ->
      failwith
        "infer_shape_resize: one and only one of sacles and sizes should be specified")
  | None     -> [| None |]


(** Main entry *)
let infer_shape_onehot input_shapes axis depth =
  match input_shapes.(0).(0) with
  | Some shp ->
    let l = Array.length shp in
    assert (axis >= -l && axis < l);
    let axis = if axis >= 0 then axis else l + axis in
    let new_shp = Owl_utils_array.insert shp [| depth |] axis in
    [| Some new_shp |]
  | None     -> [| None |]


let infer_shape_eye_like input_shape =
  match input_shape.(0).(0) with
  | Some shp ->
    assert (Array.length shp = 2);
    [| Some shp |]
  | None     -> [| None |]


let infer_shape_multinomial input_shapes sample_size =
  match input_shapes.(0).(0) with
  | Some shp ->
    assert (Array.length shp = 2);
    [| Some [| shp.(0); sample_size |] |]
  | None     -> [| None |]


let infer_shape_dynamic_quantize input_shapes =
  match input_shapes.(0).(0) with
  | Some shp -> [| Some shp; Some [||]; Some [||] |]
  | None     -> [| None; None; None |]


(* The input_shapes type is int array optin array array 
 * `int array` for a real shape;
 * `int array option` means this shape could be null;
 * `int array option array` is one whole input from previous node;
 * `int array option array array` for multiple input nodes. 
 *)

let infer_shape input_shapes sym =
  match sym with
  | Int _                   -> [| Some [||] |]
  | Float _                 -> [| Some [||] |]
  | Complex _               -> [| Some [||] |]
  | Pi _                    -> [| Some [||] |]
  | Tensor _                ->
    let shp = Owl_symbolic_symbol.shape sym in
    [| Some shp |]
  | Variable _              ->
    let shp = Owl_symbolic_symbol.shape sym in
    [| Some shp |]
  | RandomUniform _         ->
    let shp = Owl_symbolic_symbol.shape sym in
    [| Some shp |]
  | RandomNormal _          ->
    let shp = Owl_symbolic_symbol.shape sym in
    [| Some shp |]
  | EyeLike _               -> infer_shape_eye_like input_shapes
  | RandomUniformLike _     -> infer_shape_01 input_shapes
  | RandomNormalLike _      -> infer_shape_01 input_shapes
  | Multinomial x           -> infer_shape_multinomial input_shapes x.sample_size
  | ConstantOfShape _       -> [| None |] (* depends on content in input node *)
  | Range _                 -> [| None |]
  | Sin _                   -> infer_shape_01 input_shapes
  | Cos _                   -> infer_shape_01 input_shapes
  | Tan _                   -> infer_shape_01 input_shapes
  | Asin _                  -> infer_shape_01 input_shapes
  | Acos _                  -> infer_shape_01 input_shapes
  | Atan _                  -> infer_shape_01 input_shapes
  | Sinh _                  -> infer_shape_01 input_shapes
  | Cosh _                  -> infer_shape_01 input_shapes
  | Tanh _                  -> infer_shape_01 input_shapes
  | Asinh _                 -> infer_shape_01 input_shapes
  | Acosh _                 -> infer_shape_01 input_shapes
  | Atanh _                 -> infer_shape_01 input_shapes
  | Sqrt _                  -> infer_shape_01 input_shapes
  | Exp _                   -> infer_shape_01 input_shapes
  | Log _                   -> infer_shape_01 input_shapes
  | Erf _                   -> infer_shape_01 input_shapes
  | Sigmoid _               -> infer_shape_01 input_shapes
  | HardSigmoid _           -> infer_shape_01 input_shapes
  | Abs _                   -> infer_shape_01 input_shapes
  | Neg _                   -> infer_shape_01 input_shapes
  | Sign _                  -> infer_shape_01 input_shapes
  | Floor _                 -> infer_shape_01 input_shapes
  | Ceil _                  -> infer_shape_01 input_shapes
  | Round _                 -> infer_shape_01 input_shapes
  | Clip _                  -> infer_shape_01 input_shapes
  | Reciprocal _            -> infer_shape_01 input_shapes
  | Relu _                  -> infer_shape_01 input_shapes
  | ThresholdedRelu _       -> infer_shape_01 input_shapes
  | PRelu _                 -> infer_shape_03 input_shapes
  | Selu _                  -> infer_shape_01 input_shapes
  | Elu _                   -> infer_shape_01 input_shapes
  | LeakyRelu _             -> infer_shape_01 input_shapes
  | Softmax _               -> infer_shape_01 input_shapes
  | LogSoftmax _            -> infer_shape_01 input_shapes
  | Softsign _              -> infer_shape_01 input_shapes
  | Softplus _              -> infer_shape_01 input_shapes
  | Add _                   -> infer_shape_03 input_shapes
  | Sub _                   -> infer_shape_03 input_shapes
  | Mul _                   -> infer_shape_03 input_shapes
  | Div _                   -> infer_shape_03 input_shapes
  | Pow _                   -> infer_shape_01 input_shapes
  | Mod _                   -> infer_shape_01 input_shapes
  | MatMul _                -> infer_shape_19 input_shapes
  | MatMulInteger _         -> infer_shape_19 input_shapes
  | QLinearMatMul _         -> infer_shape_ql_matmul input_shapes
  | Gemm x                  -> infer_shape_gemm x input_shapes
  | Max _                   -> infer_shape_31 input_shapes
  | Min _                   -> infer_shape_31 input_shapes
  | Sum _                   -> infer_shape_31 input_shapes
  | Mean _                  -> infer_shape_31 input_shapes
  | CumSum x                -> infer_shape_axis input_shapes x.axis
  | Hardmax x               -> infer_shape_axis input_shapes x.axis
  | Det _                   -> infer_shape_det input_shapes
  | Expand _                -> infer_shape_03 input_shapes
  | And _                   -> infer_shape_03 input_shapes
  | Or _                    -> infer_shape_03 input_shapes
  | Not _                   -> infer_shape_03 input_shapes
  | Xor _                   -> infer_shape_03 input_shapes
  | Greater _               -> infer_shape_03 input_shapes
  | Less _                  -> infer_shape_03 input_shapes
  | Equal _                 -> infer_shape_03 input_shapes
  | BitShift _              -> infer_shape_03 input_shapes
  | ReduceSum x             -> infer_shape_10 input_shapes x.axes x.keepdims
  | ReduceMax x             -> infer_shape_10 input_shapes x.axes x.keepdims
  | ReduceMin x             -> infer_shape_10 input_shapes x.axes x.keepdims
  | ReduceMean x            -> infer_shape_10 input_shapes x.axes x.keepdims
  | ReduceSumSquare x       -> infer_shape_10 input_shapes x.axes x.keepdims
  | ReduceProd x            -> infer_shape_10 input_shapes x.axes x.keepdims
  | ReduceLogSum x          -> infer_shape_10 input_shapes x.axes x.keepdims
  | ReduceLogSumExp x       -> infer_shape_10 input_shapes x.axes x.keepdims
  | ReduceL1 x              -> infer_shape_10 input_shapes x.axes x.keepdims
  | ReduceL2 x              -> infer_shape_10 input_shapes x.axes x.keepdims
  | Reshape x               -> [| Some x.shape |]
  | Identity x              ->
    let idx = x.idx in
    [| input_shapes.(0).(idx) |]
  | Split x                 -> infer_shape_08 input_shapes x.axis x.split
  | Concat x                -> infer_shape_07 input_shapes x.axis
  | Pad x                   -> infer_shape_pad x input_shapes
  | Cast _                  -> infer_shape_01 input_shapes
  | Squeeze x               -> infer_shape_squeeze input_shapes x.axes
  | UnSqueeze x             -> infer_shape_unsqueeze input_shapes x.axes
  | Tile x                  -> infer_shape_05 input_shapes x.repeats
  | Shape _                 -> infer_shape_32 input_shapes
  | Size _                  -> infer_shape_33 input_shapes
  | Transpose x             -> infer_shape_transpose input_shapes x
  | Slice x                 -> infer_shape_slice input_shapes x
  | SpaceToDepth x          -> infer_shape_space_to_depth input_shapes x.blocksize
  | DepthToSpace x          -> infer_shape_depth_to_space input_shapes x.blocksize
  | IsNaN _                 -> infer_shape_01 input_shapes
  | IsInf _                 -> infer_shape_01 input_shapes
  | NonZero _               -> [| None |]
  | Where _                 -> infer_shape_34 input_shapes
  | ScatterElements _       -> infer_shape_scatter_elements input_shapes
  | ScatterND _             -> infer_shape_scatter_nd input_shapes
  | GatherElements _        -> infer_shape_gather_elements input_shapes
  | GatherND _              -> [| None |]
  | Compress x              -> infer_shape_compress input_shapes x.axis
  | ReverseSeq _            -> infer_shape_revseq input_shapes
  | Unique _                -> [| None |]
  | Resize x                -> infer_shape_resize input_shapes x.scales x.sizes
  | OneHot x                -> infer_shape_onehot input_shapes x.axis x.depth
  | Conv x                  -> infer_shape_conv input_shapes x
  | ConvTranspose x         -> infer_shape_conv_transpose input_shapes x
  | MaxPool x               ->
    infer_shape_pool ~typ:`max input_shapes x.kernel_shp x.strides x.auto_pad x.pads
  | AveragePool x           ->
    infer_shape_pool ~typ:`avg input_shapes x.kernel_shp x.strides x.auto_pad x.pads
  | BatchNormalization _    -> infer_shape_batch_normalization input_shapes
  | InstanceNorm _          -> infer_shape_instance_norm input_shapes
  | Dropout _               ->
    let t = infer_shape_01 input_shapes in
    [| t.(0); t.(0) |]
  | GlobalAveragePool _     -> infer_shape_35 input_shapes
  | GlobalMaxPool _         -> infer_shape_35 input_shapes
  | Flatten x               -> infer_shape_flatten input_shapes x.axis
  | LSTM _                  -> infer_shape_lstm input_shapes
  | RNN _                   -> infer_shape_rnn input_shapes
  | GRU _                   -> infer_shape_rnn input_shapes
  | RoiAlign x              -> infer_shape_roialign input_shapes x
  | NonMaxSuppression _     -> infer_shape_non_max_suppression input_shapes
  | QuantizeLinear _        -> infer_shape_01 input_shapes
  | DeQuantizeLinear _      -> infer_shape_01 input_shapes
  | DynamicQuantizeLinear _ -> infer_shape_dynamic_quantize input_shapes
  | SequenceEmpty _         -> [||] (* how to differ empty seq to a scalar? *)
  | SequenceAt x            -> infer_shape_seq_at input_shapes x.pos
  | SequenceInsert x        -> infer_shape_seq_insert input_shapes x.pos
  | SequenceLength _        -> [||]
  | SequenceConstruct _     -> infer_shape_seq_cons input_shapes
  | SequenceErase x         -> infer_shape_seq_erase input_shapes x.pos
  | SplitToSequence x       -> infer_shape_split_to_seq input_shapes x
  | ConcatFromSequence x    -> infer_shape_concat_from_seq input_shapes x.axis x.new_axis
  | _                       -> [| None |]
